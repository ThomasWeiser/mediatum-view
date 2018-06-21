
begin;

drop schema if exists api cascade;
drop schema if exists debug cascade;

create schema if not exists api;
create schema if not exists debug;
create schema if not exists aux;


create type api.folder as (
    id int4,
	parent_id int4,
    "name" text,
	orderpos int4,
	is_toplevel bool,
	is_collection bool,
	num_subfolder integer
);

comment on type api.folder is
    'A collection or directory, located within a hierarchy';
comment on column api.folder.id is
    'The mediaTUM node id of this collection or directory';
comment on column api.folder.name is
    'The name of the collection or directory';


create type api.metadatatype as (
    id int4,
    "name" text,
    longname text,
    datatypes text,
    description text,
    bibtexmapping text,
    citeprocmapping text
);

comment on type api.metadatatype is
    'A meta data type';
comment on column api.metadatatype.id is
    'The mediaTUM node id of the meta data type';
comment on column api.metadatatype.name is
    'The name of the meta data type';
-- TODO: COMMENT remaining columns


create type api.metafield as (
	id int4,
	metadatatype_id int4,
	name text,
	orderpos int4,
    description text,
	label text,
	"type" text
);

comment on type api.metafield is
    'A field of a meta data type';
comment on column api.metafield.metadatatype_id is
    'The mediaTUM node id of the corresponding meta data type';
comment on column api.metafield.id is
    'The mediaTUM node id of the field';
comment on column api.metafield.name is
    'The name of the field';
-- TODO: COMMENT remaining columns


create type api.mask as (
	id int4,
	metadatatype_id int4,
	name text,
	orderpos int4,
	masktype text,
    language text,
    description text,
    is_default boolean,
    -- TODO: Poss. use extra entity mask(item)type
	is_vgroup boolean
);

    
create type api.maskitem as (
	id int4,
	parent_id int4,
	name text,
	orderpos int4,
	type text,
	fieldtype text,
    width int4,
    is_required boolean
);

    
create type api.maskitem_reachable as (
	id int4,
	mask_id int4,
	metadatatype_id int4,
	superitem_id int4,
	depth integer,
	name text,
	orderpos int4,
	type text,
	fieldtype text,
    width int4,
    is_required boolean
);

    
create type api.document as (
	id int4,
	"type" text,
	"schema" text,
	"name" text,
	orderpos int4
);


drop type if exists aux.ranked_id;
create type aux.ranked_id as (
    id int4,
    rank float4
);


create type debug.mediatum_node as (
	id int4,
	"type" varchar,
	"schema" varchar,
	"name" varchar,
	orderpos int4,
	fulltext varchar,
	subnode bool
);


commit;
