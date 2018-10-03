
-- Publicly exposed GraphQL functions
-- regarding document objects.


begin;


create or replace function api.all_documents (folder_id int4, type text, name text)
    returns setof api.document as $$
    select document.*
    from entity.document
    join aux.node_lineage on document.id = node_lineage.descendant
    where folder_id = node_lineage.ancestor
    and (all_documents.type is null or document.type = all_documents.type)
    and (all_documents.name is null or document.name = all_documents.name);
$$ language sql stable rows 10000;

/*
Actually, we would like to declare that the first parameter is required.

This can be done by annotating the function as `strict` and using default arguments for the
optional parameters. See https://github.com/graphile/postgraphile/issues/438

Unfortunately, this leads to inefficient query execution.
See http://www.postgresonline.com/journal/archives/163-STRICT-on-SQL-Function-Breaks-In-lining-Gotcha.html

We tested with PostgreSQL version 9.6.5
Maybe a later of PostgreSQL version will fix this inefficiency.
Then we may want to amend the API here.

create or replace function api.all_documents (folder_id int4, type text='', name text='')
    returns setof api.document as $$
    select document.*
    from entity.document
    join aux.node_lineage on document.id = node_lineage.descendant
    where folder_id = node_lineage.ancestor
    and (all_documents.type = '' or document.type = all_documents.type)
    and (all_documents.name = '' or document.name = all_documents.name);
$$ language sql stable strict rows 10000;
*/

comment on function api.all_documents (folder_id int4, type text, name text) is
    'Reads and enables pagination through all documents in a folder, optionally filtered by type and name.';


create or replace function api.document_by_id (id int4)
    returns api.document as $$
    select *
    from entity.document
    where document.id = document_by_id.id
$$ language sql stable;

comment on function api.document_by_id (id int4) is
    'Gets a document by its mediaTUM node id.';


create or replace function api.document_attrs (document api.document, keys text[])
    returns jsonb as $$
    select aux.get_node_attrs (document.id, keys)
$$ language sql stable;

comment on function api.document_attrs (document api.document, keys text[]) is
    'Gets the node attributes of this document as a JSON value, optionally filtered by a list of keys.';


create or replace function api.document_system_attrs (document api.document, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attrs (document.id, keys)
$$ language sql stable;

comment on function api.document_system_attrs (document api.document, keys text[]) is
    'Gets the node system attributes of this document as a JSON value, optionally filtered by a list of keys.';


create or replace function api.document_metadatatype (document api.document)
    returns api.metadatatype as $$
    select api.metadatatype_by_name (document.schema)
$$ language sql stable;

comment on function api.document_metadatatype (document api.document) is
    'Gets the meta data type of this document.';


create or replace function api.metadatatype_documents (mdt api.metadatatype, type text, name text)
    returns setof api.document as $$
    select document.*
    from entity.node as document
    where document.schema = mdt.name
      and (metadatatype_documents.type is null or document.type = metadatatype_documents.type)
      and (metadatatype_documents.name is null or document.name = metadatatype_documents.name)
$$ language sql stable;

comment on function api.metadatatype_documents (mdt api.metadatatype, type text, name text) is
    'Reads and enables pagination through all documents having this meta data type, optionally filtered by type and name.';


create or replace function api.document_values_by_mask (document api.document, mask_name text)
    returns jsonb as $$
    select v.values
    from entity.document_mask_value_list as v
    where v.document_id = document_values_by_mask.document.id
      and v.mask_name = document_values_by_mask.mask_name
$$ language sql stable parallel safe;

comment on function api.document_values_by_mask (document api.document, mask_name text) is
    'Gets the meta field values of this document as a JSON value, selected by a named mask.';


create or replace function aux.fts_ordered (fts_query tsquery, domain text, language text)
    returns table (id int4, distance float4, count integer) as $$
    begin
        return query
        select fts.nid as id
             , fts.tsvec <=> fts_query as distance
             , (count(*) over ())::integer
        from mediatum.fts
        where fts.tsvec @@ fts_query
          and fts.searchtype = domain
          and fts.config = language
        order by fts.tsvec <=> fts_query;
    end;
$$ -- Using language plpgsql is more efficient here than sql.
   language plpgsql stable strict parallel safe rows 1000;


create or replace function aux.fts_document_folder_limited
    ( folder_id int4
    , fts_query tsquery
    , domain text
    , language text
    , "limit" integer
    )
    returns table
        ( document api.document
        , distance float4
        ) as $$
    select
        (document.id, document.type, document.schema, document.name, document.orderpos)::api.document as document,
        fts.distance
    from (select fts.nid as id
               , fts.tsvec <=> fts_query as distance
               , (count(*) over ())::integer
           from mediatum.fts
           where fts.tsvec @@ fts_query
           and fts.searchtype = domain
           and fts.config = language
           order by fts.tsvec <=> fts_query
         ) as fts
    join entity.document on document.id = fts.id
    where aux.test_node_lineage (folder_id, document.id)
    -- order by fts.distance -- Order obtained from subquery is preserved.
    limit "limit"
    ;
$$ -- Language sql gives stable performance here.
   -- Language plpgsql gives efficient performace for the first 5 queries
   --                  and degrades badly afterwards.
    language sql stable parallel safe rows 100;


create or replace function aux.fts_document_folder_paginated
    ( folder_id int4
    , text text
    , domain text
    , language text
    , "limit" integer
    , "offset" integer
    )
    returns table
        ( document api.document
        , distance float4
        , number integer
        , has_next_page boolean
        )
    as $$
        begin return query
            select f.document
                , f.distance
                , (row_number () over ())::integer as number
                , (count(*) over ()) > "limit" + "offset"
            from aux.fts_document_folder_limited
                ( folder_id
                , plainto_tsquery (language::regconfig, text)
                , domain
                , language
                , "limit" + "offset" + 1
                ) as f
            limit "limit"
            offset "offset";
        end;
$$ language plpgsql stable parallel safe rows 100;


create or replace function api.folder_simple_search
    (folder api.folder
    , text text
    , domain text
    , language text
    , "limit" integer default 10
    , "offset" integer default 0
    )
    returns api.fts_document_result_page as $$

        with search_result as (
                select *
                from aux.fts_document_folder_paginated
                    (folder.id, text, domain, language, "limit", "offset")
            )
        select
            coalesce(
                (select every(has_next_page) from search_result), false
            ) as has_next_page,
            array (
            select row(number, distance, document)::api.fts_document_result
                from search_result
            ) as content
        ;
$$ language sql stable parallel safe;

-- The same function as plpgsql
-- Performance behavior seems to be the same.
create or replace function api.folder_simple_search_pl
    (folder api.folder
    , text text
    , domain text
    , language text
    , "limit" integer default 10
    , "offset" integer default 0
    )
    returns api.fts_document_result_page as $$
    declare res api.fts_document_result_page;

    begin
        select
            coalesce
                ( bool_or (has_next_page)
                , false
                ) as has_next_page,
            coalesce
                ( array_agg (row (number, distance, document)::api.fts_document_result)
                , array[]::api.fts_document_result[]
                ) as content
        into res
        from
            aux.fts_document_folder_paginated (folder.id, text, domain, language, "limit", "offset")
        ;
        return res;
    end;
$$ language plpgsql stable parallel safe;

comment on function api.folder_simple_search (folder api.folder, text text, domain text, language text, "limit" integer, "offset" integer) is
    'Reads and enables pagination through all documents within a folder, filtered by a keyword search, and sorted by a search rank.'
    ' Language may currently be "english" and "german".'
    ' Domain may currently be "fulltext" and "attrs".'
    ' For pagination you may specify a limit (defaults to 10) and an offset (defaults to 0).'
    ;


----------------------------------------------------


create or replace function api.folder_author_search (folder api.folder, text text)
    returns setof api.document as $$
    select
        node.id,
        node.type,
        node.schema,
        node.name,
        node.orderpos
    from to_tsquery ('german', text) as tsq, -- needs a wellformed tsquery string
         -- to_tsquery ('german', text || ':*') as tsq, -- works only for a single word (i.e. without spaces)
         -- plainto_tsquery ('german', text) as tsq, -- no prefix search
         mediatum.node
    where aux.test_node_lineage (folder.id, node.id)
      and mediatum.to_tsvector_safe (
            'german'::regconfig,
            replace (node.attrs ->> 'author.surname', ';', ' ')
          )
          @@ tsq;
$$ language sql stable rows 100 parallel safe;

comment on function api.folder_author_search (folder api.folder, text text) is
    'Reads and enables pagination through all documents within a folder, filtered by a keyword search though the documents'' author.';


commit;
