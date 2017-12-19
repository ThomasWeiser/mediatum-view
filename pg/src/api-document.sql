

begin;


create or replace function api.all_documents (type text, name text)
    returns setof api.document as $$
    select * from entity.document
    where (all_documents.type is null or document.type = all_documents.type)
      and (all_documents.name is null or document.name = all_documents.name)
$$ language sql stable rows 10000;


create or replace function api.document_by_id (id int4)
    returns api.document as $$
    select * from entity.document
    where document.id = document_by_id.id
$$ language sql stable;


create or replace function api.document_attrs(document api.document, keys text[])
    returns jsonb as $$
    select aux.get_node_attrs(document.id, keys)
$$ language sql stable;


create or replace function api.document_system_attrs(document api.document, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attrs(document.id, keys)
$$ language sql stable;


create or replace function api.document_metadatatype(document api.document)
    returns api.metadatatype as $$
    select api.metadatatype_by_name (document.schema)
$$ language sql stable;


create or replace function api.metadatatype_documents(mdt api.metadatatype, type text, name text)
    returns setof api.document as $$
    select document.*
    from entity.node as document
    where document.schema = mdt.name
      and (metadatatype_documents.type is null or document.type = metadatatype_documents.type)
      and (metadatatype_documents.name is null or document.name = metadatatype_documents.name)
$$ language sql stable;


create or replace function api.document_values_by_mask(document api.document, mask_name text)
    returns jsonb as $$
    select v.values
    from entity.document_mask_value_list as v
    where v.document_id = document_values_by_mask.document.id
      and v.mask_name = document_values_by_mask.mask_name
$$ language sql stable parallel safe;


create or replace function aux.simple_search_hit (text text, language text, domain text, "limit" integer)
    returns setof aux.ranked_id as $$
    select fts.nid, ts_rank_cd(tsvec, tsq)
    from plainto_tsquery(language::regconfig, text) as tsq
    join mediatum.fts on fts.tsvec @@ tsq
    where fts.config = language
      and fts.searchtype = domain
    limit "limit"
$$ language sql stable parallel safe;


create or replace function aux.simple_search_hit_union (text text, languages text [], domains text [], "limit" integer)
    returns setof aux.ranked_id as $$
    select id, max(rank)
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


create or replace function api.simple_search (text text, languages text [], domains text [], "limit" integer)
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
    order by hit.rank desc
    limit "limit"
    ;
$$ language sql stable rows 100 parallel safe;


create or replace function api.simple_search_unranked (text text, languages text [], domains text [], "limit" integer)
    returns setof api.document as $$
    select document.*
    from aux.simple_search_hit_union (
            text,
            -- TODO: Should probably read list of languages from current settings.
            coalesce (languages, array['english', 'german']),
            coalesce (domains, array['fulltext', 'attrs']),
            "limit"
        ) as hit
    join entity.document on document.id = hit.id;
$$ language sql stable rows 100 parallel safe;


create or replace function api.author_search (text text) 
    returns setof api.document as $$
    select
        node.id,
        node.type,
        node.schema,
        node.name,
        node.orderpos
    from to_tsquery('german', text) as tsq, -- needs a wellformed tsquery string
         -- to_tsquery('german', text || ':*') as tsq, -- works only for a single word (i.e. without spaces)
         -- plainto_tsquery('german', text) as tsq, -- no prefix search
         mediatum.node
    where
        mediatum.to_tsvector_safe(
            'german'::regconfig,
            replace(node.attrs ->> 'author.surname', ';', ' ')
        )
        @@ tsq;
$$ language sql stable rows 100 parallel safe;


commit;
