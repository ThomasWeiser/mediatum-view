


truncate preprocess.ufts, preprocess.aspect;

------------------------------------

-- Suppress notices about "Word is too long to be indexed. Words longer than 2047 characters are ignored."
-- We don't mind that such long lexemes don't get indexed.
set session client_min_messages to warning;

insert into preprocess.ufts (nid, "year", recency, tsvec)
    select * 
    from preprocess.ufts_as_view
    -- where nid > 601000 and nid < 602000 -- For testing: process some well-known documents only
    -- where nid > 1515316
    -- limit 2000 -- For testing the code one may just process a small fraction of the data
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
    -- where nid > 601000 and nid < 602000 -- For testing: process some well-known documents only
    -- where nid > 1515316
    -- limit 44000 -- For testing: process only a smaller number of rows
;

-- Reset message level to default
set session client_min_messages to notice;

analyze preprocess.aspect;
