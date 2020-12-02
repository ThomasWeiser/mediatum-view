
-- Preprocess data for FTS and create appropriate indexes

drop schema if exists preprocess cascade;
create schema if not exists preprocess;


create table preprocess.ufts (
	nid int4 primary key references mediatum.node(id) on delete cascade,
	"year" int4 null,
	recency int4 null,
	tsvec tsvector null
);

create or replace function preprocess.year_from_attrs
    ( attrs jsonb
    )
    returns int4 as $$
    begin
        return
            -- Try to extract a 4 digits year substring from different possible date formats used in attribute field "year"
            substring((attrs #>> '{year}') from '\d{4}')::int4;
        
        exception when others then
                raise notice 'invalid year: "%".  returning null.', (attrs #>> '{year}');
                return null;
    end;
$$ language plpgsql immutable;

create or replace function preprocess.unified_tsvec_from_attrs_and_fulltext
    ( attrs jsonb
    , fulltext varchar
    )
    returns tsvector as $$
    declare
        exc_text text;
        exc_detail text;
        exc_hint text;
    begin
	    return
			setweight(to_tsvector('english_german', attrs), 'A')
			||
			setweight(to_tsvector(
                'english_german'
                -- Limit the fulltext length to avoid "string is too long for tsvector (... bytes, max 1048575 bytes)"
                , left(fulltext, 1048000)
                ), 'D');

        exception
            when others then
                get stacked diagnostics exc_text = message_text,
                                        exc_detail = pg_exception_detail,
                                        exc_hint = pg_exception_hint;
                raise notice 'Exception:  %\n%\nHint: %', exc_text, exc_detail, exc_hint;

                return null;
    end;
$$ language plpgsql immutable;


create or replace view preprocess.ufts_as_view as
    select
    node.id as id,
    preprocess.year_from_attrs(node.attrs) as "year",
    - node.id as recency,
    preprocess.unified_tsvec_from_attrs_and_fulltext(node.attrs, node.fulltext) as tsvec
    from mediatum.node
;


-- Suppress notices about "Word is too long to be indexed. Words longer than 2047 characters are ignored."
-- We don't mind that such long lexemes don't get indexed.
set session client_min_messages to warning;

insert into preprocess.ufts (nid, "year", recency, tsvec)
  select * 
  from preprocess.ufts_as_view
  -- limit 2000 -- For testing the code one may just process a small fraction of the data
;

-- Reset message level to default
set session client_min_messages to notice;

-- Index for queryies ordered by distance between tsvector and tsquery
create index if not exists ufts_rum_tsvector_ops
    on preprocess.ufts
 using rum (tsvec rum_tsvector_ops);

-- Index for queryies ordered by recency
create index if not exists ufts_rum_tsvector_addon_ops
    on preprocess.ufts
 using rum (tsvec rum_tsvector_addon_ops, recency)
  with (attach ='recency', to = 'tsvec');


analyze preprocess.ufts;

------------------------------------------------------------------

-- Possible index on table noderelation

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
