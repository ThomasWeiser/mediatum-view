
-- Define additional indexes on mediatum tables


-- Map searchtypes (aka as domains) to ranking weight classes.
-- This function is used to build the functional indexes (aka. indexes on expressions).
-- The same function has to used in the queries in order to bring the index into being used.

create or replace function set_tsvec_searchtype_weight (tsvec tsvector, searchtype text)
    returns tsvector as $$
    declare result tsvector;
    begin
        case searchtype
        	when 'attrs' then
	            result := setweight(tsvec, 'A');
            when 'fulltext' then
	            result := setweight(tsvec, 'D');
            else
	            result := tsvec;
        end case;
        return result;
    end;
$$ language plpgsql immutable;


create index if not exists fts_english_rum_tsvector_ops
    on mediatum.fts
 using rum (set_tsvec_searchtype_weight(tsvec, searchtype) rum_tsvector_ops)
 where config = 'english'::text;

create index if not exists fts_german_rum_tsvector_ops
    on mediatum.fts
 using rum (set_tsvec_searchtype_weight(tsvec, searchtype) rum_tsvector_ops)
 where config = 'german'::text;

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
