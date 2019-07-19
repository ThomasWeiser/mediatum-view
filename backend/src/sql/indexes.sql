
-- Preprocess data for FTS and create appropriate indexes

drop schema if exists preprocess cascade;
create schema if not exists preprocess;


drop table if exists preprocess.ufts;

create table preprocess.ufts (
	nid serial not null primary key references mediatum.node(id) on delete cascade,
	"year" int4 null,
	tsvec tsvector null
);

create or replace function preprocess.year_from_attrs (attrs jsonb)
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

create or replace function preprocess.unified_tsvec_from_attrs_and_fulltext (attrs jsonb, fulltext varchar)
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
    preprocess.unified_tsvec_from_attrs_and_fulltext(node.attrs, node.fulltext) as tsvec
    from mediatum.node;


insert into preprocess.ufts (nid, "year", tsvec)
  select * 
  from preprocess.ufts_as_view
  where "year" is not null or tsvec is not null
  -- limit 2000 -- For testing the code just process a tiny fraction of the data
;

create index if not exists ufts_rum_tsvector_ops
    on preprocess.ufts
 using rum (tsvec rum_tsvector_ops);


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
