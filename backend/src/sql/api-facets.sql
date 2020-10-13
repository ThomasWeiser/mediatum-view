
-- Publicly exposed GraphQL functions
-- regarding facetted search.

create or replace function api.all_documents_docset
    ( folder_id int4
    , type text default 'use null instead of this surrogate dummy'
    , name text default 'use null instead of this surrogate dummy'
    , attribute_tests api.attribute_test[] default '{}'
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
        and (all_documents_docset.type = 'use null instead of this surrogate dummy' or document.type = all_documents_docset.type)
        and (all_documents_docset.name = 'use null instead of this surrogate dummy' or document.name = all_documents_docset.name)
        and (attribute_tests = '{}' or aux.jsonb_test_list (document.attrs, attribute_tests))
        ;
        return res;
    end;
$$ language plpgsql strict stable parallel safe;


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
    , attribute_tests api.attribute_test[] default '{}'
    )
    returns api.docset
    as $$ 
    begin  
        return aux.fts_documents_tsquery_docset
          ( folder_id
          , aux.custom_to_tsquery (text)
          , nullif(attribute_tests, '{}')
          );
    end;
$$ language plpgsql strict stable parallel safe;

comment on function api.fts_documents_docset
    ( folder_id int4
    , text text
    , attribute_tests api.attribute_test[]
    ) is
    'Perform a full-text search to provide a list of document ids, '
    'intended to be consumed by a facet-counting function.'
;


create or replace function api.docset_subfolder_counts
    ( docset api.docset
    , parent_folder_id int4 default null
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
            where folder.parent_id = coalesce (parent_folder_id, docset.folder_id)
        ;
    end;
$$ language plpgsql stable parallel safe rows 50;


comment on function api.docset_subfolder_counts
    ( docset api.docset
    , parent_folder_id int4
    ) is
    'Count the distribution of the docset in relation to the subfolders of a folder.'
;
    

create or replace function api.docset_facet_by_metadatatype
    ( docset api.docset
    )
    returns setof api.facet_value as $$
        select 
            document.schema,
            count(document.schema)::integer
        from entity.document
        where document.id = ANY (docset.id_list)
        group by document.schema
        order by count(document.schema) desc, document.schema
        ;
$$ language sql stable parallel safe rows 50;


comment on function api.docset_facet_by_metadatatype
    ( docset api.docset
    ) is
    'Gather the most frequent values of the metadatatype name within the docset. '
;


create or replace function api.docset_facet_by_metadatatype_longname
    ( docset api.docset
    )
    returns setof api.facet_value as $$
        select
            aux.normalize_facet_value(metadatatype.longname),
            count(aux.normalize_facet_value(metadatatype.longname))::integer
        from entity.document
        join entity.metadatatype on document.schema = metadatatype.name
        where document.id = ANY (docset.id_list)
        group by aux.normalize_facet_value(metadatatype.longname)
        order by count(aux.normalize_facet_value(metadatatype.longname)) desc, aux.normalize_facet_value(metadatatype.longname)
        ;
$$ language sql stable parallel safe rows 50;


comment on function api.docset_facet_by_metadatatype_longname
    ( docset api.docset
    ) is
    'Gather the most frequent values of the metadatatype longname within the docset. '
    'In case of a missing longname the value is indicated as the empty string.'
;


create or replace function api.docset_facet_by_key
    ( docset api.docset
    , key text
    )
    returns setof api.facet_value as $$
        select
            aux.normalize_facet_value(document.attrs ->> key),
            count(aux.normalize_facet_value(document.attrs ->> key))::integer
        from entity.document
        where document.id = ANY (docset.id_list)
        group by aux.normalize_facet_value(document.attrs ->> key)
        order by count(aux.normalize_facet_value(document.attrs ->> key)) desc, aux.normalize_facet_value(document.attrs ->> key)
        ;
$$ language sql strict stable parallel safe rows 50;

comment on function api.docset_facet_by_key
    ( docset api.docset
    , key text
    ) is
    'Gather the most frequent values of a facet within the docset. '
    'The facet in question is specified by a JSON attribute key. '
    'Documents without this key indicate the value as the empty string.'
;


create or replace function api.docset_facet_by_mask
    ( docset api.docset
    , mask_name text
    , maskitem_name text
    )
    returns setof api.facet_value as $$
        select
            aux.normalize_facet_value(v.value),
            count(aux.normalize_facet_value(v.value))::integer
        from entity.document_mask_fields as v
        where v.document_id = ANY (docset.id_list)
        and v.mask_name = docset_facet_by_mask.mask_name
        and v.maskitem_name = docset_facet_by_mask.maskitem_name
        group by aux.normalize_facet_value(v.value)
        order by count(aux.normalize_facet_value(v.value)) desc, aux.normalize_facet_value(v.value)
        ;
$$ language sql strict stable parallel safe rows 50;

comment on function api.docset_facet_by_mask
    ( docset api.docset
    , mask_name text
    , maskitem_name text
    ) is
    'Gather the most frequent values of a facet within the docset. '
    'The facet in question is specified by maskName and maskitemName. '
    'Documents without the corresponding key indicate the value as the empty string.'
;


create or replace function api.all_documents_facet_by_key
    ( folder_id int4
    , key text
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    )
    returns setof api.facet_value as $$
        select
            aux.normalize_facet_value(document.attrs ->> key),
            count (aux.normalize_facet_value(document.attrs ->> key))::integer
        from entity.document
        join aux.node_lineage on document.id = node_lineage.descendant
        where folder_id = node_lineage.ancestor
        and (all_documents_facet_by_key.type is null or document.type = all_documents_facet_by_key.type)
        and (all_documents_facet_by_key.name is null or document.name = all_documents_facet_by_key.name)
        and (attribute_tests is null or aux.jsonb_test_list (document.attrs, attribute_tests))
        group by aux.normalize_facet_value(document.attrs ->> key)
        order by count(aux.normalize_facet_value(document.attrs ->> key)) desc, aux.normalize_facet_value(document.attrs ->> key)
        ;
$$ language sql stable rows 10000;

comment on function api.all_documents_facet_by_key
    ( folder_id int4
    , key text
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    ) is
    'Gather the most frequent values of a facet within all documents of a folder. '
    'The facet in question is specified by a JSON attribute key. '
    'Documents without this key indicate the value as the empty string. '
    'Note: When dealing with large sets of documents, this function may be faster '
    'than composing the functions "all_documents_docset" and "docset_facet_by_key". '
    'On the other hand, with smaller sets of documents, it may be slower.'
;


/* Alternatively we may mark the function as strict in order to mark the required arguments.
   Performance seems equivalent here. But see also notes on function api.all_documents_facet_by_mask_strict.
*/
create or replace function api.all_documents_facet_by_key_strict
    ( folder_id int4
    , key text
    , type text default 'use null instead of this surrogate dummy'
    , name text default 'use null instead of this surrogate dummy'
    , attribute_tests api.attribute_test[] default '{}'
    )
    returns setof api.facet_value as $$
        select
            aux.normalize_facet_value(document.attrs ->> key),
            count (aux.normalize_facet_value(document.attrs ->> key))::integer
        from entity.document
        join aux.node_lineage on document.id = node_lineage.descendant
        where folder_id = node_lineage.ancestor
        and (all_documents_facet_by_key_strict.type = 'use null instead of this surrogate dummy' or document.type = all_documents_facet_by_key_strict.type)
        and (all_documents_facet_by_key_strict.name = 'use null instead of this surrogate dummy' or document.name = all_documents_facet_by_key_strict.name)
        and (attribute_tests = '{}' or aux.jsonb_test_list (document.attrs, attribute_tests))
        group by aux.normalize_facet_value(document.attrs ->> key)
        order by count(aux.normalize_facet_value(document.attrs ->> key)) desc, aux.normalize_facet_value(document.attrs ->> key)
        ;
$$ language sql strict stable rows 10000;

comment on function api.all_documents_facet_by_key_strict
    ( folder_id int4
    , key text
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    ) is
    '@deprecated '
    'Experimental version of function allDocumentsFacetByKey, '
    'having the appropriate parameters marked as required. '
;


create or replace function api.all_documents_facet_by_mask
    ( folder_id int4
    , mask_name text
    , maskitem_name text
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    )
    returns setof api.facet_value as $$
        select
            aux.normalize_facet_value(v.value),
            count(aux.normalize_facet_value(v.value))::integer
        from entity.document_mask_fields as v
        join aux.node_lineage on v.document_id = node_lineage.descendant
        where folder_id = node_lineage.ancestor
        and (all_documents_facet_by_mask.type is null or v.document_type = all_documents_facet_by_mask.type)
        and (all_documents_facet_by_mask.name is null or v.document_name = all_documents_facet_by_mask.name)
        and (attribute_tests is null or aux.jsonb_test_list (v.document_attrs, attribute_tests))
        and v.mask_name = all_documents_facet_by_mask.mask_name
        and v.maskitem_name = all_documents_facet_by_mask.maskitem_name
        group by aux.normalize_facet_value(v.value)
        order by count(aux.normalize_facet_value(v.value)) desc, aux.normalize_facet_value(v.value)
        ;
$$ language sql stable rows 10000;

comment on function api.all_documents_facet_by_mask
    ( folder_id int4
    , mask_name text
    , maskitem_name text
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    ) is
    'Gather the most frequent values of a facet within all documents of a folder. '
    'The facet in question is specified by maskName and maskitemName. '
    'Documents without the corresponding key indicate the value as the empty string. '
    'Note: When dealing with large sets of documents, this function may be faster '
    'than composing the functions "all_documents_docset" and "docset_facet_by_mask". '
    'On the other hand, with smaller sets of documents, it may be slower.'
;

/* We would like to mark the function as strict in order to mark the required arguments.
   Unfortunately, this results in heavily degraded performance.
*/
create or replace function api.all_documents_facet_by_mask_strict
    ( folder_id int4
    , mask_name text
    , maskitem_name text
    , type text default 'use null instead of this surrogate dummy'
    , name text default 'use null instead of this surrogate dummy'
    , attribute_tests api.attribute_test[] default '{}'
    )
    returns setof api.facet_value as $$
        select
            aux.normalize_facet_value(v.value),
            count(aux.normalize_facet_value(v.value))::integer
        from entity.document_mask_fields as v
        join aux.node_lineage on v.document_id = node_lineage.descendant
        where folder_id = node_lineage.ancestor
        and (all_documents_facet_by_mask_strict.type = 'use null instead of this surrogate dummy' or v.document_type = all_documents_facet_by_mask_strict.type)
        and (all_documents_facet_by_mask_strict.name = 'use null instead of this surrogate dummy' or v.document_name = all_documents_facet_by_mask_strict.name)
        and (attribute_tests = '{}' or aux.jsonb_test_list (v.document_attrs, attribute_tests))
        and v.mask_name = all_documents_facet_by_mask_strict.mask_name
        and v.maskitem_name = all_documents_facet_by_mask_strict.maskitem_name
        group by aux.normalize_facet_value(v.value)
        order by count(aux.normalize_facet_value(v.value)) desc, aux.normalize_facet_value(v.value)
        ;
$$ language sql strict stable rows 10000;

comment on function api.all_documents_facet_by_mask_strict
    ( folder_id int4
    , mask_name text
    , maskitem_name text
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    ) is
    '@deprecated '
    'Experimental version of function allDocumentsFacetByMask, '
    'having the appropriate parameters marked as required. '
    'Performance may be degraded. '
;

