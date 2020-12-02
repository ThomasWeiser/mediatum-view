
-- Publicly exposed GraphQL functions
-- regarding full-text search.


create or replace function aux.fts_limited_by_rank
    ( folder_id int4
    , fts_query tsquery
    , aspect_internal_tests aux.aspect_internal_tests
    , attribute_tests api.attribute_test[]
    , "limit" integer
    )
    returns table
        ( id int4
        , distance float4
        , recency int4
        , year int4
        )
    as $$
    select ufts.nid as id
            , ufts.tsvec <=> fts_query as distance
            , ufts.recency as recency
            , ufts.year as year
        from preprocess.ufts
        where ufts.tsvec @@ fts_query
        and exists (select 1 from mediatum.noderelation where nid = folder_id and cid = ufts.nid)
        and aux.check_aspect_internal_tests (ufts.nid, aspect_internal_tests)
        and exists
            (select 1
             from entity.document
             where document.id = ufts.nid
             and (attribute_tests is null or 
                  aux.jsonb_test_list (document.attrs, attribute_tests)
                 )
            )           
        -- The operator <=> is provided by the RUM extension.
        -- See https://github.com/postgrespro/rum#common-operators-and-functions
        order by ufts.tsvec <=> fts_query
        limit "limit"
    ;
$$ language sql stable parallel safe rows 100;

create or replace function aux.fts_limited_by_date
    ( folder_id int4
    , fts_query tsquery
    , aspect_internal_tests aux.aspect_internal_tests
    , attribute_tests api.attribute_test[]
    , "limit" integer
    )
    returns table
        ( id int4
        , distance float4
        , recency int4
        , year int4
        )
    as $$
    select ufts.nid as id
            , ufts.tsvec <=> fts_query as distance
            , ufts.recency as recency
            , ufts.year as year
        from preprocess.ufts
        where ufts.tsvec @@ fts_query
        and exists (select 1 from mediatum.noderelation where nid = folder_id and cid = ufts.nid)           
        and aux.check_aspect_internal_tests (ufts.nid, aspect_internal_tests)
        and exists
            (select 1
             from entity.document
             where document.id = ufts.nid
             and (attribute_tests is null or 
                  aux.jsonb_test_list (document.attrs, attribute_tests)
                 )
            )
        -- The operator |=> is provided by the RUM extension.
        -- See https://github.com/postgrespro/rum#common-operators-and-functions
        order by ufts.recency |=> -2147483647
        limit "limit"
    ;
$$ language sql stable parallel safe rows 100;


create or replace function aux.fts_limited
    ( folder_id int4
    , fts_query tsquery
    , aspect_internal_tests aux.aspect_internal_tests
    , attribute_tests api.attribute_test[]
    , sorting api.fts_sorting
    , "limit" integer
    )
    returns table
        ( id int4
        , distance float4
        , recency int4
        , year int4
        )
    as $$
            select *
                from aux.fts_limited_by_date(
                        folder_id, fts_query, aspect_internal_tests, attribute_tests, "limit")
                where sorting = 'by_date'
            union
            select *
                from aux.fts_limited_by_rank(
                        folder_id, fts_query, aspect_internal_tests, attribute_tests, "limit")
                where sorting = 'by_rank'
$$ language sql stable parallel safe rows 100;


create or replace function aux.fts_paginated
    ( folder_id int4
    , text text
    , aspect_internal_tests aux.aspect_internal_tests
    , attribute_tests api.attribute_test[]
    , sorting api.fts_sorting
    , "limit" integer
    , "offset" integer
    )
    returns table
        ( id int4
        , distance float4
        , recency int4
        , year int4
        , has_next_page boolean
        )
    as $$
            select f.id
                , -- Note that the RUM extension may return a distance of 'Infinity'
                  -- which is a valid value of float4, but not a valid JSON number.
                  -- Therefore we replace 'Infinity' with the approximate maximum
                  -- valid float4 value.
                  least (f.distance, 3.4028235e+38::float4)
                , f.recency
                , f.year
                , (count(*) over ()) > "limit" + "offset"
            from aux.fts_limited
                ( folder_id
                , aux.custom_to_tsquery (text)
                    && aspect_internal_tests.combined_tsqu
                , aspect_internal_tests
                , attribute_tests
                , sorting
                , "limit" + "offset" + 1
                ) as f
            limit "limit"
            offset "offset"
$$ language sql stable parallel safe rows 10;


create or replace function aux.fts_documents_paginated
    ( folder_id int4
    , text text
    , aspect_tests api.aspect_test[]
    , attribute_tests api.attribute_test[]
    , sorting api.fts_sorting
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
        select (d.id, d.type, d.schema, d.name, d.orderpos, d.attrs)::api.document as document
            , f.distance
            , f.recency
            , f.year
            , ("offset" + 
               row_number () over 
                (order by
                    case sorting when 'by_date' then f.recency
                                 when 'by_rank' then f.distance
                    end
                )
                )::integer as number
            , f.has_next_page
        from aux.fts_paginated
            ( folder_id
            , text
            , aux.internalize_aspect_tests (aspect_tests)
            , attribute_tests
            , sorting
            , "limit"
            , "offset"
            ) as f
        join entity.document as d on d.id = f.id
        order by
            case sorting when 'by_date' then f.recency
                         when 'by_rank' then f.distance
            end
$$ language sql stable parallel safe rows 100;


-- Static SQL version. Not used due to degraded performance.
create or replace function debug.fts_documents_page_static_sql
    ( folder_id int4
    , text text
    , aspect_tests api.aspect_test[] default '{}'
    , attribute_tests api.attribute_test[] default '{}'
    , sorting api.fts_sorting default 'by_rank'
    , "limit" integer default 10
    , "offset" integer default 0
    )
    returns api.document_result_page as $$

        with search_result as (
                select *
                from aux.fts_documents_paginated
                    ( folder_id
                    , text
                    , aspect_tests
                    , attribute_tests
                    , sorting
                    , "limit", "offset"
                    )
            )
        select
            "offset" as "offset",
            coalesce(
                (select every(has_next_page) from search_result), false
            ) as has_next_page,
            array (
            select row(number, distance, recency, year, document)::api.document_result
                from search_result
            ) as content
        ;
$$ language sql strict stable parallel safe;

-- PLpgSQL version. Not used due to degraded performance.
create or replace function debug.fts_documents_page_plpgsql
    ( folder_id int4
    , text text
    , aspect_tests api.aspect_test[] default '{}'
    , attribute_tests api.attribute_test[] default '{}'
    , sorting api.fts_sorting default 'by_rank'
    , "limit" integer default 10
    , "offset" integer default 0
    )
    returns api.document_result_page as $$
    declare res api.document_result_page;

    begin
        select
            "offset",
            coalesce
                ( bool_or (has_next_page)
                , false
                ) as has_next_page,
            coalesce
                ( array_agg (row (number, distance, recency, year, document)::api.document_result)
                , array[]::api.document_result[]
                ) as content
        into res
        from
            aux.fts_documents_paginated
                ( folder_id
                , text
                , aspect_tests
                , attribute_tests
                , sorting
                , "limit", "offset"
                )
        ;
        return res;
    end;
$$ language plpgsql strict stable parallel safe;


create or replace function api.fts_documents_page
    ( folder_id int4
    , text text
    , aspect_tests api.aspect_test[] default '{}'
    , attribute_tests api.attribute_test[] default '{}'
    , sorting api.fts_sorting default 'by_rank'
    , "limit" integer default 10
    , "offset" integer default 0
    )
    returns api.document_result_page as $$
    declare res api.document_result_page;

    begin
        -- We use dynamic SQL execution here in order to avoid generic plan caching.
        execute
            'select'
            '    $7,'
            '    coalesce'
            '        ( bool_or (has_next_page)'
            '        , false'
            '        ) as has_next_page,'
            '    coalesce'
            '        ( array_agg (row (number, distance, recency, year, document)::api.document_result)'
            '        , array[]::api.document_result[]'
            '        ) as content'
            '  from'
            '    aux.fts_documents_paginated'
            '        ( $1'
            '        , $2'
            '        , $3'
            '        , $4'
            '        , $5'
            '        , $6, $7'
            '        )'
            ';'
        into strict res
        using folder_id, text, aspect_tests, attribute_tests, sorting, "limit", "offset"
        ;
        return res;
    end;
$$ language plpgsql strict stable parallel safe;

comment on function api.fts_documents_page (folder_id int4, text text, aspect_tests api.aspect_test[], attribute_tests api.attribute_test[], sorting api.fts_sorting, "limit" integer, "offset" integer) is
    'Perform a full-text-search on the documents of a folder, sorted by a search rank,'
    ' optionally filtered by a list of aspect tests and a list of attribute tests.'
    ' Sorting of the results is either "by_rank" (default) or "by_date".'
    ' For pagination you may specify a limit (defaults to 10) and an offset (defaults to 0).'
    ;


----------------------------------------------------


create or replace function api.author_search
    ( folder_id int4
    , text text
    )
    returns setof api.document as $$
    select
        node.id,
        node.type,
        node.schema,
        node.name,
        node.orderpos,
        node.attrs
    from aux.custom_to_tsquery (text) as tsq,
         mediatum.node
    where aux.test_node_lineage (folder_id, node.id)
      and mediatum.to_tsvector_safe (
            'german'::regconfig,
            replace (node.attrs ->> 'author.surname', ';', ' ')
          )
          @@ tsq;
$$ language sql strict stable rows 100 parallel safe;

comment on function api.author_search (folder_id int4, text text) is
    'Reads and enables pagination through all documents within a folder, filtered by a keyword search though the documents'' author.';
