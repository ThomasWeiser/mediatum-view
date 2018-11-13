
-- Publicly exposed GraphQL functions
-- regarding facetted search.

begin;


create or replace function aux.fts_folder_docset
    ( folder_id int4
    , fts_query tsquery
    , domain text
    , language text
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
        from ( select fts.nid as id
               from mediatum.fts
               where fts.tsvec @@ fts_query
               and fts.searchtype = domain
               and fts.config = language
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


create or replace function api.folder_fts_docset
    ( folder api.folder
    , text text
    , domain text
    , language text
    , attribute_tests api.attribute_test[]
    )
    returns api.docset
    as $$ 
    begin  
        return aux.fts_folder_docset
          ( folder.id
          , plainto_tsquery (language::regconfig, text)
          , domain
          , language
          , attribute_tests
          );
    end;
$$ language plpgsql stable parallel safe;

comment on function api.folder_fts_docset
    ( folder api.folder
    , text text
    , domain text
    , language text
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
    , "limit" integer
    )
    returns setof api.facet_value as $$
        select document.attrs ->> key, count(document.attrs ->> key)::integer
        from entity.document
        where document.id = ANY (docset.id_list)
        group by document.attrs ->> key
        order by count(document.attrs ->> key) desc, document.attrs ->> key
        limit "limit"
        ;
$$ language sql stable parallel safe rows 50;

comment on function api.docset_facet_by_key
    ( docset api.docset
    , key text
    , "limit" integer
    ) is
    'Gather the most frequent values of a facet within the docset. '
    'The facet in question is specified by a JSON attribute key.'
;


create or replace function api.docset_facet_by_mask
    ( docset api.docset
    , mask_name text
    , maskitem_name text
    , "limit" integer
    )
    returns setof api.facet_value as $$
        select v.value, count(v.value)::integer
        from entity.document_mask_fields as v
        where v.document_id = ANY (docset.id_list)
        and v.mask_name = docset_facet_by_mask.mask_name
        and v.maskitem_name = docset_facet_by_mask.maskitem_name
        group by v.value
        --order by node.attrs ->> 'year'
        order by count(v.value) desc, v.value
        limit "limit"
        ;
$$ language sql stable parallel safe rows 50;

comment on function api.docset_facet_by_mask
    ( docset api.docset
    , mask_name text
    , maskitem_name text
    , "limit" integer
    ) is
    'Gather the most frequent values of a facet within the docset. '
    'The facet in question is specified by maskName and maskitemName.'
;


commit;
