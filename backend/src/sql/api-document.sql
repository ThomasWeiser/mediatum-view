
-- Publicly exposed GraphQL functions
-- regarding document objects.


create or replace function aux.all_documents_limited
    ( folder_id int4
    , type text
    , name text
    , aspect_internal_tests aux.aspect_internal_tests
    , attribute_tests api.attribute_test[]
    , "limit" integer
    )
    returns setof api.document
    as $$
    -- 1. We use dynamic SQL here in order to avoid performance degradation from generic plan caching.
    -- 2. TODO: If aspect tests are given, then perform a FTS on combined aspect values.
    -- 3. TODO: Similar FTS queries currently run much faster. So, there may be some optimizations possible here.
    --          Maybe like this: Inner query for sorting (index only access), outer query to access other document columns.
	begin
		return query execute
        'select document.id, document.type, document.schema, document.name, document.orderpos, document.attrs'
        '    from entity.document'
        '    join aux.node_lineage on document.id = node_lineage.descendant'
        '    where $1 = node_lineage.ancestor'
        '    and ($2 is null or document.type = $2)'
        '    and ($3 is null or document.name = $3)'
        '    and aux.check_aspect_internal_tests (document.id, $4)'
        '    and ($5 = ''{}'' or aux.jsonb_test_list (document.attrs, $5))'
        '    order by document.id desc'
        '    limit $6'
        using
            folder_id,
            all_documents_limited.type,
            all_documents_limited.name,
            aspect_internal_tests,
            attribute_tests,
            "limit";
    end;
$$ language plpgsql stable parallel safe rows 100;


create or replace function aux.all_documents_paginated
    ( folder_id int4
    , type text
    , name text
    , aspect_internal_tests aux.aspect_internal_tests
    , attribute_tests api.attribute_test[]
    , "limit" integer
    , "offset" integer
    )
    returns table
        ( document api.document
        , distance float4
        , recency int4
        , year int4
        , number integer
        , has_next_page boolean
        )
    as $$
        begin 
            return query
            select f
                , 0.0::float4
                , - f.id -- Same definition of "recency" as in table proprocess.ufts!
                , preprocess.year_from_attrs (f.attrs)
                , (row_number () over ())::integer as number
                , (count(*) over ()) > "limit" + "offset"
            from aux.all_documents_limited
                ( folder_id
                , type
                , name
                , aspect_internal_tests
                , attribute_tests
                , "limit" + "offset" + 1
                ) as f
            limit "limit"
            offset "offset";
        end;
$$ language plpgsql stable parallel safe rows 100;


create or replace function api.all_documents_page
    ( folder_id int4
    , type text default 'use null instead of this surrogate dummy'
    , name text default 'use null instead of this surrogate dummy'
    , aspect_tests api.aspect_test[] default '{}'
    , attribute_tests api.attribute_test[] default '{}'
    , "limit" integer default 10
    , "offset" integer default 0
    )
    returns api.document_result_page as $$

        with search_result as (
                select *
                from aux.all_documents_paginated
                    ( folder_id
                    , nullif(type, 'use null instead of this surrogate dummy')
                    , nullif(name, 'use null instead of this surrogate dummy')
                    , aux.internalize_aspect_tests (aspect_tests)
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
            select row(number, distance, recency, year, document)::api.document_result
                from search_result
            ) as content
        ;
$$ language sql strict stable parallel safe;




/*
TODO: Update comment: We now have mixed required/optional arguments, by declaring the function as `strict` and using default arguments.

Actually, we would like to declare that the first parameter is required.

This can be done by annotating the function as `strict` and using default arguments for the
optional parameters. See https://github.com/graphile/postgraphile/issues/438

Unfortunately, this leads to inefficient query execution because of not inlining the function when declared as `strict`.
See http://www.postgresonline.com/journal/archives/163-STRICT-on-SQL-Function-Breaks-In-lining-Gotcha.html

PostgreSQL is very rigorous about inlining `strict` functions:
https://wiki.postgresql.org/wiki/Inlining_of_SQL_functions

    If the function is declared STRICT, then the planner must be able to prove that the body expression necessarily
    returns NULL if any parameter is null. At present, this condition is only satisfied if: every parameter is referenced
    at least once, and all functions, operators and other constructs used in the body are themselves STRICT.

It seems too hard to meet these requirements.
As a consequnce it's currently not possible to to declare some of the parameters as required.
*/


comment on function api.all_documents_page (folder_id int4, type text, name text, aspect_tests api.aspect_test[] , attribute_tests api.attribute_test[], "limit" integer, "offset" integer) is
    'Reads and enables pagination through all documents in a folder, optionally filtered by type and name and a list of aspect and attribute tests';


create or replace function api.document_by_id
    ( id int4
    )
    returns api.document as $$
    select *
    from entity.document
    where document.id = document_by_id.id
$$ language sql strict stable;

comment on function api.document_by_id (id int4) is
    'Gets a document by its mediaTUM node id.';


create or replace function api.document_folders
    ( document api.document
    )
    returns api.folder[] as $$
    select array(
        select row(folder.*)::api.folder
        from entity.folder
        join mediatum.nodemapping on folder.id = nodemapping.nid
        where document.id = nodemapping.cid
        order by folder.orderpos
    )
$$ language sql stable;

comment on function api.document_folders (document api.document) is
    'Gets the list of all folders in which the document appears.';


create or replace function api.document_attributes
    ( document api.document
    , keys text[]
    )
    returns jsonb as $$
    select aux.get_document_attributes (document, keys)
$$ language sql stable;

comment on function api.document_attributes (document api.document, keys text[]) is
    'Gets the node attributes of this document as a JSON value, optionally filtered by a list of keys.';


create or replace function api.document_system_attributes
    ( document api.document
    , keys text[]
    )
    returns jsonb as $$
    select aux.get_node_system_attributes (document.id, keys)
$$ language sql stable;

comment on function api.document_system_attributes (document api.document, keys text[]) is
    'Gets the node system attributes of this document as a JSON value, optionally filtered by a list of keys.';


create or replace function api.document_metadatatype
    ( document api.document
    )
    returns api.metadatatype as $$
    select api.metadatatype_by_name (document.schema)
$$ language sql stable;

comment on function api.document_metadatatype (document api.document) is
    'Gets the meta data type of this document.';


create or replace function api.metadatatype_documents
    ( mdt api.metadatatype
    , type text
    , name text
    )
    returns setof api.document as $$
    select document.*
    from entity.node as document
    where document.schema = mdt.name
      and (metadatatype_documents.type is null or document.type = metadatatype_documents.type)
      and (metadatatype_documents.name is null or document.name = metadatatype_documents.name)
$$ language sql stable;

comment on function api.metadatatype_documents (mdt api.metadatatype, type text, name text) is
    'Reads and enables pagination through all documents having this meta data type, optionally filtered by type and name.';


create or replace function api.document_from_search
    ( document api.document
    , text text
    )
    returns api.document_from_search as $$
    select 
        document,
        aux.convert_to_or_query(aux.custom_to_tsquery (text))
$$ language sql strict stable parallel safe;

comment on function api.document_from_search (document api.document, text text) is
    'Attach a search term that was used to find this document. '
    'Utilized to get search-related annotations on the document.';


create or replace function api.document_from_search_fulltext_matching
    ( document_from_search api.document_from_search
    )
    returns boolean as $$
    select exists
        (select
            from preprocess.ufts
            where ufts.tsvec @@ aux.setweight (document_from_search.tsquery, 
                -- Note that we use weight 'D' for indexing fulltext.
                'D')
            and ufts.nid = (document_from_search).document.id
        )
$$ language sql strict stable parallel safe;

comment on function api.document_from_search_fulltext_matching (document_from_search api.document_from_search) is
    'Checks whether the associated search term occurs in the fulltext of the document.';


create or replace function api.document_from_search_attributes_matching
    ( document_from_search api.document_from_search
    )
    returns boolean as $$
    select exists
        (select
            from preprocess.ufts
            where ufts.tsvec @@ aux.setweight (document_from_search.tsquery,
                -- Note that we use weight 'A' for indexing attributes.
                -- Later we may also use the weights 'B' and 'C' for attributes.
                'ABC')
            and ufts.nid = (document_from_search).document.id
        )
$$ language sql strict stable parallel safe;

comment on function api.document_from_search_attributes_matching (document_from_search api.document_from_search) is
    'Checks whether the associated search term occurs in the attributes of the document.';


create or replace function api.document_values_by_mask
    ( document api.document
    , mask_name text
    , highlight_text text default ''
    )
    returns jsonb as $$
    select
        jsonb_agg (
            jsonb_build_object (
                'field',
                metafield_name,
                'name',
                maskitem_name,
                'width',
                maskitem_width,
                'value',
                case when highlight_text = '' then
                    value
                else
                    ts_headline
                        ( value
                        , aux.custom_to_tsquery (highlight_text)
                        , aux.ts_headline_options (true)
                        )
                end
            )
        )
    from (select * from entity.document_mask_fields order by maskitem_orderpos) as e
    where e.document_id = document_values_by_mask.document.id
    and   e.mask_name = document_values_by_mask.mask_name
$$ language sql strict stable parallel safe;


comment on function api.document_values_by_mask (document api.document, mask_name text, highlightText text) is
    'Gets the meta field values of this document as a JSON value, selected by a named mask. '
    'Optionally mark the occurences of a search term given as highlightText.';
