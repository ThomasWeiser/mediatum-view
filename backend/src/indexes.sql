
-- Define additional indexes on mediatum tables


create index if not exists fts_fulltext_english_rum_tsvector_ops
    on mediatum.fts
 using rum (tsvec rum_tsvector_ops)
 where config = 'english'::text
   and searchtype = 'fulltext'::text;

create index if not exists fts_fulltext_german_rum_tsvector_ops
    on mediatum.fts
 using rum (tsvec rum_tsvector_ops)
 where config = 'german'::text
   and searchtype = 'fulltext'::text;

create index if not exists fts_attrs_english_rum_tsvector_ops
    on mediatum.fts
 using rum (tsvec rum_tsvector_ops)
 where config = 'english'::text
   and searchtype = 'attrs'::text;

create index if not exists fts_attrs_german_rum_tsvector_ops
    on mediatum.fts
 using rum (tsvec rum_tsvector_ops)
 where config = 'german'::text
   and searchtype = 'attrs'::text;

