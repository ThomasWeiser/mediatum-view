
-- Publicly exposed GraphQL functions
-- regarding full-text search.

create or replace function aux.fts_ordered
    ( fts_query tsquery
    )
    returns table
        ( id int4
        , distance float4
        , year int4
        , count integer
        ) as $$
    begin
        return query
        select ufts.nid as id
             , ufts.tsvec <=> fts_query as distance
             , ufts.year as year
             , (count(*) over ())::integer
        from preprocess.ufts
        where ufts.tsvec @@ fts_query
        order by ufts.tsvec <=> fts_query;
    end;
$$ -- Using language plpgsql is more efficient here than sql.
   language plpgsql stable strict parallel safe rows 1000;


create or replace function aux.fts_documents_limited
    ( folder_id int4
    , fts_query tsquery
    , attribute_tests api.attribute_test[]
    , sorting api.fts_sorting
    , "limit" integer
    )
    returns table
        ( document api.document
        , distance float4
        , year int4
        )
    as $$
    select
        (document.id, document.type, document.schema, document.name, document.orderpos, document.attrs)::api.document as document,
        fts.distance,
        year
    from (select ufts.nid as id
               , ufts.tsvec <=> fts_query as distance
               , ufts.year as year
               , (count(*) over ())::integer
           from preprocess.ufts
           where ufts.tsvec @@ fts_query
           
           order by
               case when sorting = 'by_date' then ufts.year <=| 2147483647
                    when sorting = 'by_rank' then ufts.tsvec <=> fts_query
               end,
               ufts.nid desc
           
         ) as fts
    join entity.document on document.id = fts.id

    -- Performance: Using exists with a subquery is faster than current implementation of aux.test_node_lineage
    -- where aux.test_node_lineage (folder_id, fts.id)
    where exists (select 1 from mediatum.noderelation where nid = folder_id and cid = fts.id)

      and (attribute_tests is null or 
           aux.jsonb_test_list (document.attrs, attribute_tests)
          )

    
    
    -- Order obtained from subquery used to be preserved.
    -- Maybe not true anymore (PostgreSQL v11.5).
    -- Behaviour seems inconsistent and fluctuating.
    -- Performance may also be degraded.
    -- Needs more investigation.

    -- For now we sort here once again.
    order by
        case when sorting = 'by_date' then fts.year <=| 2147483647 
             when sorting = 'by_rank' then fts.distance
        end,
        fts.id desc
    
    limit "limit"
    ;
$$ -- Language sql gives stable performance here.
   -- Language plpgsql gives efficient performace for the first 5 queries
   --                  and degrades badly afterwards.
    language sql stable parallel safe rows 100;


create or replace function aux.fts_documents_paginated
    ( folder_id int4
    , text text
    , attribute_tests api.attribute_test[]
    , sorting api.fts_sorting
    , "limit" integer
    , "offset" integer
    )
    returns table
        ( document api.document
        , distance float4
        , year int4
        , number integer
        , has_next_page boolean
        )
    as $$
        begin return query
            select f.document
                , f.distance
                , f.year
                , (row_number () over ())::integer as number
                , (count(*) over ()) > "limit" + "offset"
            from aux.fts_documents_limited
                ( folder_id
                , aux.custom_to_tsquery (text)
                , attribute_tests
                , sorting
                , "limit" + "offset" + 1
                ) as f
            limit "limit"
            offset "offset";
        end;
$$ language plpgsql stable parallel safe rows 100;


create or replace function api.fts_documents_page
    ( folder_id int4
    , text text
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
                    , nullif(attribute_tests, '{}')
                    , sorting
                    , "limit", "offset"
                    )
            )
        select
            "offset",
            coalesce(
                (select every(has_next_page) from search_result), false
            ) as has_next_page,
            array (
            select row(number, distance, year, document)::api.document_result
                from search_result
            ) as content
        ;
$$ language sql strict stable parallel safe;

comment on function api.fts_documents_page (folder_id int4, text text, attribute_tests api.attribute_test[], sorting api.fts_sorting, "limit" integer, "offset" integer) is
    'Perform a full-text-search on the documents of a folder, sorted by a search rank, optionally filtered by type and name and a list of attribute tests.'
    ' Sorting of the results is either "by_rank" (default) or "by_date".'
    ' For pagination you may specify a limit (defaults to 10) and an offset (defaults to 0).'
    ;

-- The same function as plpgsql
-- Performance behavior seems to be the same.
create or replace function api.fts_documents_page_pl
    ( folder_id int4
    , text text
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
                ( array_agg (row (number, distance, year, document)::api.document_result)
                , array[]::api.document_result[]
                ) as content
        into res
        from
            aux.fts_documents_paginated
                ( folder_id
                , text
                , nullif(attribute_tests, '{}')
                , sorting
                , "limit", "offset"
                )
        ;
        return res;
    end;
$$ language plpgsql strict stable parallel safe;

comment on function api.fts_documents_page_pl (folder_id int4, text text, attribute_tests api.attribute_test[], sorting api.fts_sorting, "limit" integer, "offset" integer) is
    '@deprecated '
    'Alternative implementation of ftsDocumentsPage; may have different perfoamce behavior. '
    ' Perform a full-text-search on the documents of a folder, sorted by a search rank, optionally filtered by type and name and a list of attribute tests.'
    ' Sorting of the results is either "by_rank" (default) or "by_date".'
    ' For pagination you may specify a limit (defaults to 10) and an offset (defaults to 0).'
    ;


----------------------------------------------------


create or replace function api.author_search (folder_id int4, text text)
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
