
-- Definitions of the types publicly exposed as GraphQL objects.


begin;

drop schema if exists api cascade;
drop schema if exists debug cascade;

create schema if not exists api;
create schema if not exists debug;


create type api.folder as (
    id int4,
	parent_id int4,
    "name" text,
	orderpos int4,
	is_collection boolean,
	num_subfolder integer
);

comment on type api.folder is
    'A collection or directory, located within a hierarchy';
comment on column api.folder.id is
    'The mediaTUM node id of this collection or directory';
comment on column api.folder.parent_id is
    'The mediaTUM node id of the parent folder; is NULL if this is a root folder.';
comment on column api.folder.name is
    'The name of the collection or directory';
comment on column api.folder.orderpos is
    'A number indicating the position of this folder among its siblings';
comment on column api.folder.is_collection is
	'A folder can be a collection (true) or a directory (false)';
comment on column api.folder.num_subfolder is
    'Number of subfolders of this folder';


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
comment on column api.metadatatype.longname is
    'The long name as given in the attributes of the meta data type';
comment on column api.metadatatype.datatypes is
    'Basic data type as given in the node attributes of the meta data type, e.g. "document", "audio" etc';
comment on column api.metadatatype.description is
    'A description as given in the node attributes of the meta data type';
comment on column api.metadatatype.bibtexmapping is
    'The bibtexmapping given in the node attributes of the meta data type';
comment on column api.metadatatype.citeprocmapping is
    'The citeprocmapping given in the node attributes of the meta data type';


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
comment on column api.metafield.id is
    'The mediaTUM node id of the field';
comment on column api.metafield.metadatatype_id is
    'The mediaTUM node id of the corresponding meta data type';
comment on column api.metafield.name is
    'The name of the field';
comment on column api.metafield.orderpos is
    'A number indicating the position of this field among its siblings';
comment on column api.metafield.description is
    'A description as given in the node attributes of the field';
comment on column api.metafield.label is
    'The label as given in the node attributes of the field';
comment on column api.metafield."type" is
    'The type given in the node attributes of the field, e.g. "text", "list" etc';


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

comment on type api.mask is
    'A mask used for a specific meta data type';
comment on column api.mask.id is
    'The mediaTUM node id of the mask';
comment on column api.mask.metadatatype_id is
    'The mediaTUM node id of the corresponding meta data type';
comment on column api.mask.name is
    'The name of the mask, e.g. "nodesmall"';
comment on column api.mask.orderpos is
    'A number indicating the position of this mask among its siblings';
comment on column api.mask.masktype is
    'The type of the mask as given in the node attributes, e.g. "edit", "fullview", "shortview" etc';
comment on column api.mask.language is
    'The language of the mask as given in the node attributes, e.g. "en", "de"';
comment on column api.mask.description is
    'A description as given in the node attributes of the mask';
comment on column api.mask.is_default is
    'The mask may be the default mask for the meta data type; given in the node attributes of the mask';
comment on column api.mask.is_vgroup is
    'The mask may be a vertical group; given in the node attributes of the mask';
    

create type api.maskitem as (
	id int4,
	parent_id int4,
	name text,
	orderpos int4,
	"type" text,
	fieldtype text,
    width int4,
    is_required boolean
);

comment on type api.maskitem is
    'An item of a specific mask or a sub-item of another mask item';
comment on column api.maskitem.id is
    'The mediaTUM node id of the item';
comment on column api.maskitem.parent_id is
    'The mediaTUM node id of the parent mask or mask item';
comment on column api.maskitem.orderpos is
    'A number indicating the position of this item among its siblings';
comment on column api.maskitem."type" is
    'The type as given in the node attributes of the item; may be "field", "label", "vgroup" or "hgroup"';
comment on column api.maskitem.fieldtype is
    'The fieldtype as given in the node attributes of the item; may be "standard", "mapping" or "attribute"';
comment on column api.maskitem.width is
    'The width of the item to be used in a UI as given in the node attributes of the item';
comment on column api.maskitem.is_required is
    'The item may be required; as given in the node attributes of the item';

    
create type api.maskitem_reachable as (
	id int4,
	mask_id int4,
	metadatatype_id int4,
	superitem_id int4,
	depth integer,
	name text,
	orderpos int4,
	"type" text,
	fieldtype text,
    width int4,
    is_required boolean
);

comment on type api.maskitem_reachable is
    'An item of a specific mask or a sub-item of another mask item; this uses an alternative implementation to "maskitem", using recursion';
comment on column api.maskitem_reachable.id is
    'The mediaTUM node id of the item';
comment on column api.maskitem_reachable.mask_id is
    'The mediaTUM node id of the corresponding mask';
comment on column api.maskitem_reachable.metadatatype_id is
    'The mediaTUM node id of the meta data type of the corresponding mask';
comment on column api.maskitem_reachable.superitem_id is
    'The mediaTUM node id of the parent mask or mask item';
comment on column api.maskitem_reachable.depth is
    'The number of hierarchy steps between this item and its corresponding mask';
comment on column api.maskitem_reachable.orderpos is
    'A number indicating the position of this item among its siblings';
comment on column api.maskitem_reachable."type" is
    'The type as given in the node attributes of the item; may be "field", "label", "vgroup" or "hgroup"';
comment on column api.maskitem_reachable.fieldtype is
    'The fieldtype as given in the node attributes of the item; may be "standard", "mapping" or "attribute"';
comment on column api.maskitem_reachable.width is
    'The width of the item to be used in a UI as given in the node attributes of the item';
comment on column api.maskitem_reachable.is_required is
    'The item may be required; as given in the node attributes of the item';

    
create type api.mapping as (
	id int4,
	name text,
	orderpos int4,
	"type" text,
    description text
);

comment on type api.mapping is
    'A mapping used e.g. for exporting the data selected by a mask';
comment on column api.mapping.id is
    'The mediaTUM node id of the mapping';
comment on column api.mapping.name is
    'The name of the mapping';
comment on column api.mapping.orderpos is
    'A number indicating the position of this mapping among its siblings';
comment on column api.mapping."type" is
    'The type as given in the node attributes of the mapping; may be "default", "citeproc", "marc21" or NULL';
comment on column api.mapping.description is
    'A description as given in the node attributes of the mapping';
    

create type api.mappingfield as (
	id int4,
	mapping_id int4,
	name text,
	orderpos int4,
    description text,
    is_mandatory boolean
);

comment on type api.mappingfield is
    'A field within a mapping used e.g. for exporting the data selected by a mask';
comment on column api.mappingfield.id is
    'The mediaTUM node id of the mapping field';
comment on column api.mappingfield.name is
    'The name of the mapping field';
comment on column api.mappingfield.orderpos is
    'A number indicating the position of this mapping field among its siblings';
comment on column api.mappingfield.description is
    'A description as given in the node attributes of the mapping field';
comment on column api.mappingfield.is_mandatory is
    'The field may be mandatory; as given in the node attributes of the mapping field';

    
create type api.document as (
	id int4,
	"type" text,
	"schema" text,
	name text,
	orderpos int4
);

comment on type api.document is
    'A document as the basic subject of publication of the media server';
comment on column api.document.id is
    'The mediaTUM node id of this document; used as a public reference';
comment on column api.document."type" is
    'The major type of the document; may be "document", "image", "video", "audio", "dissertation" or "other"';
comment on column api.document."schema" is
    'The schema of the document is the name of the corresponding meta data type';
comment on column api.document.name is
    'The name given to the document, usually the author''s name';
comment on column api.document.orderpos is
    'A number indicating the position of this document among its siblings';


create type api.fts_document_result as (
    number integer,
    distance float4,
    document api.document
);

comment on type api.fts_document_result is
    'A single result from a full text search, containing a document';
comment on column api.fts_document_result.number is
    'Sequence number of this result';
comment on column api.fts_document_result.distance is
    'A measure of the relevance of this result with respect to the query expression; lower values means higher relevance';
comment on column api.fts_document_result.document is
    'The resulting document';


create type api.fts_document_result_page as (
    has_next_page boolean,
    content api.fts_document_result[]
);

comment on type api.fts_document_result_page is
    'A result page from a full text search of documents';
comment on column api.fts_document_result_page.has_next_page is
    'Indicates whether there are more results after the current page';
comment on column api.fts_document_result_page.content is
    'A list of document results from a full text search';


create type api.docset as (
    folder_id int4,
    count integer,
    id_list int4[]
);

create type api.folder_count as (
    folder_id int4,
    count integer
);

create type debug.mediatum_node as (
	id int4,
	"type" varchar,
	"schema" varchar,
	"name" varchar,
	orderpos int4,
	fulltext varchar,
	subnode boolean
);


commit;
