
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


/* Add this index only if proven useful.
   Up to now we don't have a query that would profit from it.
   Caution: I've experienced some performance loss _with_ the index,
            when checking for node lineage using a `exists` sub-query on noderelation.
            Without this index the planner uses the pkey index and is a bit faster.

-- We have already a multi-column index (nid, cid, distance) from primary key.
-- For enumerating parents we add a multi-column index (cid, nid) here.
create index if not exists ix_mediatum_noderelation_cid_nid
    on noderelation
 using btree (cid, nid);
*/
