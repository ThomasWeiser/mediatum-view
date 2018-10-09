begin;


-- drop type if exists aux.ranked_id;
create type aux.ranked_id as (
    id int4,
    rank float4
);


create or replace function aux.simple_search_hit (text text, language text, domain text, "limit" integer)
    returns setof aux.ranked_id as $$
    select fts.nid, ts_rank_cd (tsvec, tsq)
    from plainto_tsquery (language::regconfig, text) as tsq
    join mediatum.fts on fts.tsvec @@ tsq
    where fts.config = language
      and fts.searchtype = domain
    limit "limit"
$$ language sql stable parallel safe;


create or replace function aux.simple_search_hit_union (text text, languages text [], domains text [], "limit" integer)
    returns setof aux.ranked_id as $$
    select id, max (rank)
    from (
        -- We enumerate all possible combinations
        -- (assuming a configuration to use the languages english and german).
        -- This way we make sure that PostgreSQL uses the corresponding partial indexes on the fts table.
        -- Alternatively we could build dynamic SQL queries instead (if fixed enumeration isn't an option).
        select * from aux.simple_search_hit (text, 'german', 'fulltext', "limit")
        where 'german' = any (languages) and  'fulltext' = any (domains)
        union all
        select * from aux.simple_search_hit (text, 'english', 'fulltext', "limit")
        where 'english' = any (languages) and  'fulltext' = any (domains)
        union all
        select * from aux.simple_search_hit (text, 'german', 'attrs', "limit")
        where 'german' = any (languages) and  'attrs' = any (domains)
        union all
        select * from aux.simple_search_hit (text, 'english', 'attrs', "limit")
        where 'english' = any (languages) and  'attrs' = any (domains)
    ) as hit
    group by id
    limit "limit"
$$ language sql stable parallel safe;


create or replace function api.folder_simple_search (folder api.folder, text text, languages text [], domains text [], "limit" integer)
    returns setof api.document as $$
    select document.*
    from aux.simple_search_hit_union (
            text,
            -- TODO: Should probably read list of languages from current settings.
            coalesce (languages, array['english', 'german']),
            coalesce (domains, array['fulltext', 'attrs']),
            "limit" * 2
        ) as hit
    join entity.document on document.id = hit.id
    where aux.test_node_lineage (folder.id, document.id)
    order by hit.rank desc
    limit "limit"
    ;
$$ language sql stable rows 100 parallel safe;

comment on function api.folder_simple_search (folder api.folder, text text, languages text [], domains text [], "limit" integer) is
    'Reads and enables pagination through all documents within a folder, filtered by a keyword search, and sorted by a search rank.'
    ' Languages may currently include "english" and "german".'
    ' Domains may currently include "fulltext" and "attrs".'
    ' You have to give a limit for the number of results (in order to limit the expense for sorting them by rank).';


create or replace function api.folder_simple_search_unranked (folder api.folder, text text, languages text [], domains text [], "limit" integer)
    returns setof api.document as $$
    select document.*
    from aux.simple_search_hit_union (
            text,
            -- TODO: Should probably read list of languages from current settings.
            coalesce (languages, array['english', 'german']),
            coalesce (domains, array['fulltext', 'attrs']),
            "limit"
        ) as hit
    join entity.document on document.id = hit.id
    where aux.test_node_lineage (folder.id, document.id)
$$ language sql stable rows 100 parallel safe;

comment on function api.folder_simple_search (folder api.folder, text text, languages text [], domains text [], "limit" integer) is
    'Reads and enables pagination through all documents within a folder, filtered by a keyword search, not sorted by a search rank.'
    ' Languages may currently include "english" and "german".'
    ' Domains may currently include "fulltext" and "attrs".'
    ' You have to give a limit for the number of results.';


create or replace function api.folder_author_search (folder api.folder, text text)
    returns setof api.document as $$
    select
        node.id,
        node.type,
        node.schema,
        node.name,
        node.orderpos
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
