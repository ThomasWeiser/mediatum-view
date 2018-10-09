
-- Publicly exposed GraphQL functions
-- regarding facetted search.

begin;


create or replace function aux.fts_folder_docset
    ( folder_id int4
    , fts_query tsquery
    , domain text
    , language text
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
        -- join entity.document on document.id = fts.id -- TODO
        where exists ( select 1
                      from mediatum.noderelation
                      where nid = folder_id and cid = fts.id
                    )
        -- and aux.is_public_today (fts.id) -- TODO
        ; 
        return res;
    end;
$$ language plpgsql stable parallel safe;


create or replace function api.folder_fts_docset
    ( folder api.folder
    , text text
    , domain text
    , language text
    )
    returns api.docset
    as $$ 
    begin  
        return aux.fts_folder_docset
          ( folder.id
          , plainto_tsquery (language::regconfig, text)
          , domain
          , language
          );
    end;
$$ language plpgsql stable parallel safe;


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


commit;
