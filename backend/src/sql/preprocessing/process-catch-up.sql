
/* Add missing rows to the tables of preprocessed data. 
   Existing rows are not updated.
   Existing rows that are no longer relevant are deleted.
*/

/* TODO:
    Currently we skip existing rows via PostgreSQL's "upsert" mechanism, i.e. "on conflict ... do nothing".
    This seems to be not as efficient as it could be, so we should consider alternative methods later.
    E.g. using temporary tables holding the idenitifiers of those nodes thawt need to be preprocessed.
*/

------------------------------------

-- Output current values of possible configuration limits

select
    current_setting('mediatum.preprocessing_min_node_id', true)::int as "preprocessing_min_node_id",
    current_setting('mediatum.preprocessing_max_node_id', true)::int as "preprocessing_max_node_id",
    current_setting('mediatum.preprocessing_limit', true)::int as "preprocessing_limit";

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
    on conflict on constraint ufts_pkey do nothing
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
    from preprocess.aspect_as_view
    where (nid >= current_setting('mediatum.preprocessing_min_node_id', true)::int) is not false
      and (nid <= current_setting('mediatum.preprocessing_max_node_id', true)::int) is not false
    limit current_setting('mediatum.preprocessing_limit', true)::int * 10
    on conflict on constraint aspect_pkey do nothing
;

-- Reset message level to default
set session client_min_messages to notice;

delete from preprocess.aspect
using config.aspect_def
where aspect.name not in (select name from config.aspect_def);

analyze preprocess.aspect;
