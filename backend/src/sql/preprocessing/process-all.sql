
------------------------------------

-- Output current values of possible configuration limits

select
    current_setting('mediatum.preprocessing_min_node_id', true)::int as "preprocessing_min_node_id",
    current_setting('mediatum.preprocessing_max_node_id', true)::int as "preprocessing_max_node_id",
    current_setting('mediatum.preprocessing_limit', true)::int as "preprocessing_limit";

------------------------------------

truncate preprocess.ufts, preprocess.aspect;

------------------------------------

-- Suppress notices about "Word is too long to be indexed. Words longer than 2047 characters are ignored."
-- We don't mind that such long lexemes don't get indexed.
set session client_min_messages to warning;

insert into preprocess.ufts (nid, year, recency, tsvec)
    select nid, year, recency, tsvec
    from preprocess.ufts_as_view
    -- For testing the code one may just process a fraction of the data
    -- by using the custom configuration parameters below
    where (nid >= current_setting('mediatum.preprocessing_min_node_id', true)::int) is not false
      and (nid <= current_setting('mediatum.preprocessing_max_node_id', true)::int) is not false
    limit current_setting('mediatum.preprocessing_limit', true)::int
;

-- Reset message level to default
set session client_min_messages to notice;

analyze preprocess.ufts;

-- Create the indexes not before now, when the tables are already filled. This is more efficient than updating the indexes row by row.

-- Index for queryies ordered by distance between tsvector and tsquery
create index if not exists ufts_rum_tsvector_ops
    on preprocess.ufts
 using rum (tsvec rum_tsvector_ops);

-- Index for queryies ordered by recency
create index if not exists ufts_rum_tsvector_addon_ops
    on preprocess.ufts
 using rum (tsvec rum_tsvector_addon_ops, recency)
  with (attach ='recency', to = 'tsvec');

------------------------------------

-- Suppress notices about "Word is too long to be indexed. Words longer than 2047 characters are ignored."
-- We don't mind that such long lexemes don't get indexed.
set session client_min_messages to warning;

insert into preprocess.aspect (nid, name, values, tsvec)
    select nid, name, values, tsvec
    from preprocess.aspect_as_view
    where (nid >= current_setting('mediatum.preprocessing_min_node_id', true)::int) is not false
      and (nid <= current_setting('mediatum.preprocessing_max_node_id', true)::int) is not false
    limit current_setting('mediatum.preprocessing_limit', true)::int * 10
;

-- Reset message level to default
set session client_min_messages to notice;

analyze preprocess.aspect;

-- Create the indexes not before now, when the tables are already filled. This is more efficient than updating the indexes row by row.

create index if not exists aspect_rum_tsvector_ops
    on preprocess.aspect
 using rum (tsvec rum_tsvector_ops)
;

create index if not exists aspect_rum_tsvector_addon_ops
    on preprocess.aspect
 using rum (tsvec rum_tsvector_addon_ops, name)
  with (attach ='name', to = 'tsvec')
 ;

-- TODO: Possibly create index on (name, values), using gin utilizing extension btree_gin
--       See https://stackoverflow.com/q/31945601

