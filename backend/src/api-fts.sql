
-- Publicly exposed GraphQL functions
-- regarding full-text search.

begin;


create or replace function aux.fts_ordered
    (fts_query tsquery
    , domain text
    , language text
    )
    returns table
        ( id int4
        , distance float4
        , count integer
        ) as $$
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
    , attribute_tests api.attribute_test[]
    , "limit" integer
    )
    returns table
        ( document api.document
        , distance float4
        ) as $$
    select
        (document.id, document.type, document.schema, document.name, document.orderpos, document.attrs)::api.document as document,
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

    -- Performance: Using exists with a subquery is faster than current implementation of aux.test_node_lineage
    -- where aux.test_node_lineage (folder_id, document.id)
    where exists (select 1 from mediatum.noderelation where nid = folder_id and cid = document.id)

      and (attribute_tests is null or 
           aux.jsonb_test_list (document.attrs, attribute_tests)
          )

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
    , attribute_tests api.attribute_test[]
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
                , attribute_tests
                    , "limit" + "offset" + 1
                ) as f
            limit "limit"
            offset "offset";
        end;
$$ language plpgsql stable parallel safe rows 100;


create or replace function api.folder_fts_page
    ( folder api.folder
    , text text
    , domain text
    , language text
    , attribute_tests api.attribute_test[]
    , "limit" integer default 10
    , "offset" integer default 0
    )
    returns api.fts_document_result_page as $$

        with search_result as (
                select *
                from aux.fts_document_folder_paginated
                    ( folder.id
                    , text, domain, language
                    , attribute_tests
                    , "limit", "offset"
                    )
            )
        select
            "offset",
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
create or replace function api.folder_fts_page_pl
    (folder api.folder
    , text text
    , domain text
    , language text
    , attribute_tests api.attribute_test[]
    , "limit" integer default 10
    , "offset" integer default 0
    )
    returns api.fts_document_result_page as $$
    declare res api.fts_document_result_page;

    begin
        select
            "offset",
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
            aux.fts_document_folder_paginated
                ( folder.id
                , text, domain, language
                , attribute_tests
                , "limit", "offset"
                )
        ;
        return res;
    end;
$$ language plpgsql stable parallel safe;

/*
comment on function api.folder_fts_page (folder api.folder, text text, domain text, language text, "limit" integer, "offset" integer) is
    'Reads and enables pagination through all documents within a folder, filtered by a keyword search, and sorted by a search rank.'
    ' Language may currently be "english" and "german".'
    ' Domain may currently be "fulltext" and "attrs".'
    ' For pagination you may specify a limit (defaults to 10) and an offset (defaults to 0).'
    ;
*/

----------------------------------------------------


create or replace function api.folder_author_search (folder api.folder, text text)
    returns setof api.document as $$
    select
        node.id,
        node.type,
        node.schema,
        node.name,
        node.orderpos,
        node.attrs
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
