
-- Define additional indexes on mediatum tables


create index if not exists fts_fulltext_rum_tsvector_ops
    on mediatum.fts
 using rum (tsvec rum_tsvector_ops)
 where searchtype = 'fulltext'::text;

create index if not exists fts_attrs_rum_tsvector_ops
    on mediatum.fts
 using rum (tsvec rum_tsvector_ops)
 where searchtype = 'attrs'::text;

create index if not exists fts_rum_tsvector_ops
    on mediatum.fts
 using rum (tsvec rum_tsvector_ops);

