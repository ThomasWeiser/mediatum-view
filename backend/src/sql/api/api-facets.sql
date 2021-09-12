
-- Publicly exposed GraphQL functions
-- regarding facetted search.

/* TODO: We should probably check in all relevant api functions
         that the given aspect tests are allowed to be used for fts or facet filtering.
         One option would be to check them against the tables config.aspect_fts and config.aspect_facet.
 */

create or replace function aux.fts_documents_tsquery_docset
    ( folder_id int4
    , fts_query tsquery
    , aspect_internal_tests aux.aspect_internal_tests
    )
    returns api.docset
    as $$ 
    declare res api.docset;
    declare fts_query_with_aspect_terms tsquery =
        fts_query && aspect_internal_tests.combined_tsqu;
    begin
        if fts_query_with_aspect_terms = ''::tsquery then
            select folder_id
                , row(folder_id, count (document.id))::api.folder_count
                , array_agg (document.id)
            into res
            from entity.document
            join aux.node_lineage on document.id = node_lineage.descendant
            where folder_id = node_lineage.ancestor
            and aux.check_aspect_internal_tests (document.id, aspect_internal_tests)
            ;
        else
            select folder_id
                , row(folder_id, count (fts.id))::api.folder_count
                , array_agg (fts.id)
            into res
            from ( select 

                        -- Eleminating duplicates: Since we use the ufts table,
                        -- which has at most one row per document, there cannot be any duplicates here.
                        -- But, removing the "distinct" modifier here slows down the query by a factor of 3.
                        -- The reason is currently unknown.
                        -- I also tried to extract this sub-query as a function (using either sql or plpgsql),
                        -- but could not get stable performance results with this approach.
                        -- We should investigate the issue later. EXPLAIN is our friend here.
                        -- For now we keep using a sub-query and using "distinct" in the sub-query.
                        distinct
                    
                        ufts.nid as id
                from preprocess.ufts
                where ufts.tsvec @@ fts_query_with_aspect_terms
                ) as fts
            join entity.document on document.id = fts.id
            where exists ( select 1
                        from mediatum.noderelation
                        where nid = folder_id and cid = fts.id
                        )
                and aux.check_aspect_internal_tests (document.id, aspect_internal_tests)
            ;
        end if;
        return res;
    end;
$$ language plpgsql strict stable parallel safe;


create or replace function api.all_documents_docset
    ( folder_id int4
    , aspect_tests api.aspect_test[] default '{}'
    )
    returns api.docset as $$
    begin
        return aux.fts_documents_tsquery_docset
          ( folder_id
          , ''::tsquery
          , aux.internalize_aspect_tests (aspect_tests)
          );
    end;
$$ language plpgsql strict stable parallel safe;


comment on function api.all_documents_docset (folder_id int4, aspect_tests api.aspect_test[]) is
    'Perform a documents listing on a folder to provide a list of document ids, '
    'intended to be consumed by a facet-counting function.'
;


create or replace function api.fts_documents_docset
    ( folder_id int4
    , text text
    , aspect_tests api.aspect_test[] default '{}'
    )
    returns api.docset
    as $$ 
    begin  
        return aux.fts_documents_tsquery_docset
          ( folder_id
          , aux.custom_to_tsquery (text)
          , aux.internalize_aspect_tests (aspect_tests)
          );
    end;
$$ language plpgsql strict stable parallel safe;

comment on function api.fts_documents_docset
    ( folder_id int4
    , text text
    , aspect_tests api.aspect_test[]
    ) is
    'Perform a full-text search to provide a list of document ids, '
    'intended to be consumed by a facet-counting function.'
;


create or replace function api.docset_subfolder_counts
    ( docset api.docset
    )
    returns setof api.folder_count as $$
    begin
        return query
            select
              folder.id,
              -- Counting in a sub-query together with `exists` is faster than
              -- counting with a group-by and together with `aux.test_node_lineage`
              -- TODO: Test if this is also true in case of a large number of sub-folders.
              ( select count (*)
                    from unnest (docset.id_list) as document_id_rows
                    where exists ( select 1
                                  from mediatum.noderelation
                                  where nid = folder.id and cid = document_id_rows
                                )
              )::integer
            from entity.folder
            where folder.parent_id = docset.folder_id
        ;
    end;
$$ language plpgsql stable parallel safe rows 50;


comment on function api.docset_subfolder_counts
    ( docset api.docset
    ) is
    'Count the distribution of the docset in relation to the subfolders of a folder.'
;
    

create or replace function api.docset_facet_by_aspect
    ( docset api.docset
    , aspect_name text
    )
    returns setof api.facet_value as $$
        select
            value,
            count(value)::integer
        from 
            preprocess.aspect,
            unnest (aspect.values) as value
        where aspect.nid = ANY (docset.id_list)
          and aspect.name = aspect_name
        group by value
        order by count(value) desc, value
        ;
$$ language sql strict stable parallel safe rows 50;

comment on function api.docset_facet_by_aspect
    ( docset api.docset
    , aspect_name text
    ) is
    'Gather the most frequent values of a facet within the docset. '
    'The facet in question is specified by an aspectName. '
    -- TODO: Determine how we will handle missing values; and describe it here in this doc.
    'Documents without the corresponding key indicate the value as the empty string.'
;

/* TODO: We should probably check that the given aspect is allowed to be used as a facet.
         One option would be to check them against the table config.aspect_facet.
 */

