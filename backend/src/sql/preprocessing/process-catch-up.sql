
/* Add missing rows to the tables of preprocessed data. 
   Existing rows are not updated.
   Existing rows that are no longer relevant are deleted.
*/

/* TODO:
    Currently we skip existing rows via PostgreSQL's "upsert" mechanism, i.e. "on conflict ... do nothing".
    This seems to be not as efficient as it could be, so we should consider alternative methods later.
    E.g. using temporary tables holding the idenitifiers of those nodes that need to be preprocessed.
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
    select ufts_as_view.nid, ufts_as_view.year, ufts_as_view.recency, ufts_as_view.tsvec
    from preprocess.ufts_as_view, preprocess.ufts_missing
    where 
    	ufts_as_view.nid = ufts_missing.nid
      and (ufts_as_view.nid >= current_setting('mediatum.preprocessing_min_node_id', true)::int) is not false
      and (ufts_as_view.nid <= current_setting('mediatum.preprocessing_max_node_id', true)::int) is not false
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
    select aspect_as_view.nid, aspect_as_view.name, aspect_as_view.values, aspect_as_view.tsvec
    from preprocess.aspect_as_view,
         preprocess.aspect_missing
    where aspect_as_view.nid = aspect_missing.nid and aspect_as_view.name = aspect_missing.name
      and (aspect_as_view.nid >= current_setting('mediatum.preprocessing_min_node_id', true)::int) is not false
      and (aspect_as_view.nid <= current_setting('mediatum.preprocessing_max_node_id', true)::int) is not false
    limit current_setting('mediatum.preprocessing_limit', true)::int * 10
    on conflict on constraint aspect_pkey do nothing
;


-- Reset message level to default
set session client_min_messages to notice;

delete from preprocess.aspect
using config.aspect_def
where aspect.name not in (select name from config.aspect_def);

analyze preprocess.aspect;
