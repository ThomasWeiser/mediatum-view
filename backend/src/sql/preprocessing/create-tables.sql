
create schema if not exists preprocess;

------------------------------------------------------------------

create table if not exists preprocess.ufts (
	nid int4 primary key references mediatum.node(id) on delete cascade,
	"year" int4 null,
	recency int4 null,
	tsvec tsvector null
);

------------------------------------------------------------------

create table if not exists preprocess.aspect (
	nid int4 references mediatum.node(id) on delete cascade,
    name text,
    values text[] not null,
	tsvec tsvector not null,
    primary key (nid, name)
);

-- Note that index creation is deferred until initial inserting of the preprocessed data.
-- This is done in `process-all.sql`.
