
-- Publicly exposed GraphQL functions
-- regarding facetted search.

create or replace function api.all_documents_docset
    ( folder_id int4
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    )
    returns api.docset as $$
    declare res api.docset;
    begin
        select folder_id
             , row(folder_id, count (document.id))::api.folder_count
             , array_agg (document.id)
        into res
        from entity.document
        join aux.node_lineage on document.id = node_lineage.descendant
        where folder_id = node_lineage.ancestor
        and (all_documents_docset.type is null or document.type = all_documents_docset.type)
        and (all_documents_docset.name is null or document.name = all_documents_docset.name)
        and (attribute_tests is null or aux.jsonb_test_list (document.attrs, attribute_tests))
        ;
        return res;
    end;
$$ language plpgsql stable parallel safe;


comment on function api.all_documents_docset (folder_id int4, type text, name text, attribute_tests api.attribute_test[]) is
    'Perform a documents listing on a folder to provide a list of document ids, '
    'intended to be consumed by a facet-counting function.'
;


create or replace function aux.fts_documents_tsquery_docset
    ( folder_id int4
    , fts_query tsquery
    , attribute_tests api.attribute_test[]
    )
    returns api.docset
    as $$ 
    declare res api.docset;
    begin  
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
               where ufts.tsvec @@ fts_query
             ) as fts
        join entity.document on document.id = fts.id
        where exists ( select 1
                      from mediatum.noderelation
                      where nid = folder_id and cid = fts.id
                     )
              and ( attribute_tests is null or 
                    aux.jsonb_test_list (document.attrs, attribute_tests)
                  )
        ; 
        return res;
    end;
$$ language plpgsql stable parallel safe;


create or replace function api.fts_documents_docset
    ( folder_id int4
    , text text
    , attribute_tests api.attribute_test[]
    )
    returns api.docset
    as $$ 
    begin  
        return aux.fts_documents_tsquery_docset
          ( folder_id
          , aux.custom_to_tsquery (text)
          , attribute_tests
          );
    end;
$$ language plpgsql stable parallel safe;

comment on function api.fts_documents_docset
    ( folder_id int4
    , text text
    , attribute_tests api.attribute_test[]
    ) is
    'Perform a full-text search to provide a list of document ids, '
    'intended to be consumed by a facet-counting function.'
;


create or replace function aux.subfolder_counts
    ( docset api.docset
    , parent_folder_id int4 default null
    )
    returns table
        ( folder_id int4
        , count integer
        ) as $$
    begin
        return query
            select
              node_children,
              -- Counting in a sub-query together with `exists` is faster than
              -- counting with a group-by and together with `aux.test_node_lineage`
              -- TODO: Test if this is also true in case of a large number of sub-folders.
              ( select count (*)
                    from unnest (docset.id_list) as document_id_rows
                    where exists ( select 1
                                  from mediatum.noderelation
                                  where nid = node_children and cid = document_id_rows
                                )
              )::integer
            from aux.node_children (coalesce (parent_folder_id, docset.folder_id))
        ;
    end;
$$ language plpgsql stable parallel safe rows 50;


create or replace function api.docset_subfolder_counts
    ( docset api.docset
    , parent_folder_id int4 -- default null
    )
    returns setof api.folder_count as $$
    begin
        return query
            select *
            from aux.subfolder_counts (docset, parent_folder_id)
        ;
    end;
$$ language plpgsql stable parallel safe rows 50;

comment on function api.docset_subfolder_counts
    ( docset api.docset
    , parent_folder_id int4
    ) is
    'Count the distribution of the docset in relation to the subfolders of a folder.'
;
    

create or replace function api.docset_facet_by_key
    ( docset api.docset
    , key text
    )
    returns setof api.facet_value as $$
        select document.attrs ->> key, count(document.attrs ->> key)::integer
        from entity.document
        where document.id = ANY (docset.id_list)
        group by document.attrs ->> key
        order by count(document.attrs ->> key) desc, document.attrs ->> key
        ;
$$ language sql stable parallel safe rows 50;

comment on function api.docset_facet_by_key
    ( docset api.docset
    , key text
    ) is
    'Gather the most frequent values of a facet within the docset. '
    'The facet in question is specified by a JSON attribute key.'
;


create or replace function api.docset_facet_by_mask
    ( docset api.docset
    , mask_name text
    , maskitem_name text
    )
    returns setof api.facet_value as $$
        select v.value, count(v.value)::integer
        from entity.document_mask_fields as v
        where v.document_id = ANY (docset.id_list)
        and v.mask_name = docset_facet_by_mask.mask_name
        and v.maskitem_name = docset_facet_by_mask.maskitem_name
        group by v.value
        order by count(v.value) desc, v.value
        ;
$$ language sql stable parallel safe rows 50;

comment on function api.docset_facet_by_mask
    ( docset api.docset
    , mask_name text
    , maskitem_name text
    ) is
    'Gather the most frequent values of a facet within the docset. '
    'The facet in question is specified by maskName and maskitemName.'
;


create or replace function api.all_documents_facet_by_key
    ( folder_id int4
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    , key text
    )
    returns setof api.facet_value as $$
        select document.attrs ->> key, count (document.attrs ->> key)::integer
        from entity.document
        join aux.node_lineage on document.id = node_lineage.descendant
        where folder_id = node_lineage.ancestor
        and (all_documents_facet_by_key.type is null or document.type = all_documents_facet_by_key.type)
        and (all_documents_facet_by_key.name is null or document.name = all_documents_facet_by_key.name)
        and (attribute_tests is null or aux.jsonb_test_list (document.attrs, attribute_tests))
        group by document.attrs ->> key
        order by count(document.attrs ->> key) desc, document.attrs ->> key
        ;
$$ language sql stable rows 10000;

comment on function api.all_documents_facet_by_key
    ( folder_id int4
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    , key text
    ) is
    'Gather the most frequent values of a facet within all documents of a folder. '
    'The facet in question is specified by a JSON attribute key.'
    'Note: When dealing with large sets of documents, this function is faster '
    'than composing the functions "all_documents_docset" and "docset_facet_by_key".'
;


create or replace function api.all_documents_facet_by_mask
    ( folder_id int4
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    , mask_name text
    , maskitem_name text
    )
    returns setof api.facet_value as $$
        select v.value, count(v.value)::integer
        from entity.document_mask_fields as v
        join aux.node_lineage on v.document_id = node_lineage.descendant
        where folder_id = node_lineage.ancestor
        and (all_documents_facet_by_mask.type is null or v.document_type = all_documents_facet_by_mask.type)
        and (all_documents_facet_by_mask.name is null or v.document_name = all_documents_facet_by_mask.name)
        and (attribute_tests is null or aux.jsonb_test_list (v.document_attrs, attribute_tests))
        and v.mask_name = all_documents_facet_by_mask.mask_name
        and v.maskitem_name = all_documents_facet_by_mask.maskitem_name
        group by v.value
        order by count(v.value) desc, v.value
        ;
$$ language sql stable rows 10000;

comment on function api.all_documents_facet_by_mask
    ( folder_id int4
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    , mask_name text
    , maskitem_name text
    ) is
    'Gather the most frequent values of a facet within all documents of a folder. '
    'The facet in question is specified by maskName and maskitemName. '
    'Note: When dealing with large sets of documents, this function is faster '
    'than composing the functions "all_documents_docset" and "docset_facet_by_mask".'
;
