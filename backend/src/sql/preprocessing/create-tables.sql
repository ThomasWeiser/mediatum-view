
create schema if not exists preprocess;

------------------------------------------------------------------

create table if not exists preprocess.ufts (
	nid int4 primary key references mediatum.node(id) on delete cascade,
	"year" int4 null,
	recency int4 null,
	tsvec tsvector null
);

-- TODO: Possibly defer index creation after filling in the of preprocessed data.

-- Index for queryies ordered by distance between tsvector and tsquery
create index if not exists ufts_rum_tsvector_ops
    on preprocess.ufts
 using rum (tsvec rum_tsvector_ops);

-- Index for queryies ordered by recency
create index if not exists ufts_rum_tsvector_addon_ops
    on preprocess.ufts
 using rum (tsvec rum_tsvector_addon_ops, recency)
  with (attach ='recency', to = 'tsvec');

------------------------------------------------------------------

create table if not exists preprocess.aspect (
	nid int4 references mediatum.node(id) on delete cascade,
    name text,
    values text[] not null,
	tsvec tsvector not null,
    primary key (nid, name)
);


-- TODO: Possibly defer index creation after filling in the preprocessed data.

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
