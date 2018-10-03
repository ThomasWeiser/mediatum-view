/*
create or replace function aux.fts_folder_all
-- Function will probably be removed.
-- We cannot easily chain set-returning functions in Postgraphile.
-- Use fts_folder_document_set instead.
    ( folder_id int4
    , fts_query tsquery
    , domain text
    , language text
    )
    returns table
        ( document_id int4
        , count integer
        ) as $$
    select fts.id
         , (count(*) over ())::integer
    from (select fts.nid as id
           from mediatum.fts
           where fts.tsvec @@ fts_query
           and fts.searchtype = domain
           and fts.config = language
         ) as fts
    -- join entity.document on document.id = fts.id
    where exists (select 1 from mediatum.noderelation where nid = folder_id and cid = fts.id)
      --and aux.is_public_today (fts.id)
    ;
$$ -- Language sql gives stable performance here.
   -- Language plpgsql gives efficient performace for the first 5 queries
   --                  and degrades badly afterwards.
    language sql stable parallel safe rows 5000;


create or replace function aux.fts_folder_subfolder_counts
-- Function will probably be removed.
-- Use fts_folder_document_set and hand the result over to folder_subfolder_counts instead.
-- Use folder_subfolder_counts after fts_folder_document_set instead.
-- The document_set should be computed once for counting different facets (besides subfolders) over the set.
    ( folder_id int4
    , fts_query tsquery
    , domain text
    , language text
    )
    returns table
        ( folder_id int4
        , count integer
        ) as $$

    select node_self_and_children,
          (count (fts.document_id))::integer
    from aux.fts_folder_all (folder_id, fts_query, domain, language) as fts
      , aux.node_self_and_children (folder_id)
    where 
      -- Suprisingly, using `test_node_lineage` is faster here than using `exists`.
      aux.test_node_lineage (node_self_and_children, fts.document_id)
      -- exists (select 1 from mediatum.noderelation where nid = node_self_and_children and cid = fts.document_id)
    group by node_self_and_children
    ;
$$ -- Language sql gives stable performance here.
   -- Language plpgsql gives efficient performace for the first 5 queries
   --                  and degrades badly afterwards.
    language sql stable parallel safe rows 5000;
*/

create or replace function aux.fts_folder_document_set
    ( folder_id int4
    , fts_query tsquery
    , domain text
    , language text
    )
    returns api.document_set
    as $$ 
    declare res api.document_set;
    begin  
    select array_agg (fts.id)
    into res
    from (select fts.nid as id
           from mediatum.fts
           where fts.tsvec @@ fts_query
           and fts.searchtype = domain
           and fts.config = language
         ) as fts
    -- join entity.document on document.id = fts.id
    where exists (select 1 from mediatum.noderelation where nid = folder_id and cid = fts.id)
    -- and aux.is_public_today (fts.id)
    ; 
    return res;
    end;
$$ language plpgsql stable parallel safe;


create or replace function aux.folder_subfolder_counts
    ( document_set api.document_set
    , folder_id int4
    )
    returns table
        ( folder_id int4
        , count integer
        ) as $$

    select
      node_self_and_children,
      -- Counting in a sub-query together with `exists` is faster than
      -- counting with a group-by and together with `aux.test_node_lineage`
      (select count (*)
            from unnest (document_set.id_list) as document_id_rows
            where exists (select 1 from mediatum.noderelation where nid = node_self_and_children and cid = document_id_rows)
      )::integer
    from aux.node_self_and_children (folder_id)
    ;
$$ language sql stable parallel safe rows 50;

