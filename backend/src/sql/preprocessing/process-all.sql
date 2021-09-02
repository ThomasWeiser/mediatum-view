
------------------------------------

-- Output possibly configured limits

select
    current_setting('mediatum.preprocessing_min_node_id', true)::int as "mediatum.preprocessing_min_node_id",
    current_setting('mediatum.preprocessing_max_node_id', true)::int as "mediatum.preprocessing_max_node_id",
    current_setting('mediatum.preprocessing_limit', true)::int as "mediatum.preprocessing_limit";

------------------------------------

truncate preprocess.ufts, preprocess.aspect;

------------------------------------

-- Suppress notices about "Word is too long to be indexed. Words longer than 2047 characters are ignored."
-- We don't mind that such long lexemes don't get indexed.
set session client_min_messages to warning;

insert into preprocess.ufts (nid, "year", recency, tsvec)
    select * 
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

------------------------------------

-- Suppress notices about "Word is too long to be indexed. Words longer than 2047 characters are ignored."
-- We don't mind that such long lexemes don't get indexed.
set session client_min_messages to warning;

insert into preprocess.aspect (nid, name, values, tsvec)
    select nid, name, values, tsvec
    from preprocess.aspect_view
    where (nid >= current_setting('mediatum.preprocessing_min_node_id', true)::int) is not false
      and (nid <= current_setting('mediatum.preprocessing_max_node_id', true)::int) is not false
    limit current_setting('mediatum.preprocessing_limit', true)::int * 10
;

-- Reset message level to default
set session client_min_messages to notice;

analyze preprocess.aspect;
