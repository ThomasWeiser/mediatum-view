
-- Definitions of the types publicly exposed as GraphQL objects.

create schema if not exists api;
create schema if not exists aux;


create type api.generic_node as
    ( id int4
    -- Add more (hidden) columns?
    );

comment on type api.generic_node is
    'A generic node, which may be a folder or a document';
comment on column api.generic_node.id is
    'The mediaTUM node id';


create type api.folder as
    ( id int4
	, parent_id int4
    , "name" text
	, orderpos int4
	, is_collection boolean
	, has_subfolder boolean
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
comment on column api.folder.has_subfolder is
    'Does this folder have at least one subfolder?';


create type api.metadatatype as
    ( id int4
    , "name" text
    , longname text
    , datatypes text
    , description text
    , bibtexmapping text
    , citeprocmapping text
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


create type api.metafield as
    ( id int4
	, metadatatype_id int4
	, name text
	, orderpos int4
    , description text
	, label text
	, "type" text
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


create type api.mask as
    ( id int4
	, metadatatype_id int4
	, name text
	, orderpos int4
	, masktype text
    , language text
    , description text
    , is_default boolean
    -- TODO: Poss. use extra entity mask(item)type
	, is_vgroup boolean
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
    

create type api.maskitem as
    ( id int4
	, parent_id int4
	, name text
	, orderpos int4
	, "type" text
	, fieldtype text
    , width int4
    , is_required boolean
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

    
create type api.maskitem_reachable as
    ( id int4
	, mask_id int4
	, metadatatype_id int4
	, superitem_id int4
	, depth integer
	, name text
	, orderpos int4
	, "type" text
	, fieldtype text
    , width int4
    , is_required boolean
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

    
create type api.mapping as
    ( id int4
	, name text
	, orderpos int4
	, "type" text
    , description text
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
    

create type api.mappingfield as
    ( id int4
	, mapping_id int4
	, name text
	, orderpos int4
    , description text
    , is_mandatory boolean
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

    
create type api.document as
    ( id int4
    , "type" text
    , "schema" text
    , name text
    , orderpos int4
    , attrs jsonb
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
comment on column api.document.attrs is
    '@omit';


create type api.document_from_search as
    ( document api.document
    , tsquery tsquery
    );

comment on type api.document_from_search is
    'A document together with a search term that was used to find the document. '
    'Utilized to get search-related annotations on the document.';
comment on column api.document_from_search.document is
    '@omit';
comment on column api.document_from_search.tsquery is
    '@omit';


create type api.file as
    ( filetype text
    , mimetype text
    );

comment on type api.file is
    'Data about a file associated with a document';
comment on column api.file.filetype is
    'Common filetype are "document", "thumb", "presentation"';
comment on column api.file.mimetype is
    'The mimetype of the file';


create type api.fts_sorting as enum
    ( 'by_rank'
    , 'by_date'
    );


create type api.attribute_test_operator as enum (
    'equality', 'equalitywithblanknull', 'ilike', 'simplefts', 'daterange'
);

create type api.attribute_test as
    ( key text
    , operator api.attribute_test_operator
    , value text
    , extra text
    );

comment on type api.attribute_test is
    'Specification for testing  a single attribute value of a document';
comment on column api.attribute_test.key is
    'Name of the attribute to be tested';
comment on column api.attribute_test.operator is
    'The test to perform; maybe "equality", "equalitywithblanknull", "ilike", "simplefts", "daterange". '
    'The test "equalitywithblanknull" succeeds also if comparison value is the empty string and the attribute value is either an emtpy or whitespace-only string or NULL.';
comment on column api.attribute_test.value is
    'Comparison value for the attribute to be tested';
comment on column api.attribute_test.extra is
    'Second comparison value, used if operator may take two values, like "ilike" or "daterange"';


create type api.aspect_test_operator as enum (
    'equality', 'fts'
);

create type api.aspect_test as
    ( name text
    , operator api.aspect_test_operator
    , value text
    );

comment on type api.aspect_test is
    'Specification for testing  a single aspect value of a document';
comment on column api.aspect_test.name is
    'Name of the aspect to be tested';
comment on column api.aspect_test.operator is
    'The test to perform; maybe "equality" or "fts".';
comment on column api.aspect_test.value is
    'Comparison value or search term for the aspect to be tested';


-- We define an internal representation of a user-defined set of aspect tests,
-- which which allows for more efficient execution of the tests
create type aux.aspect_internal_test_equality as
    ( name text
    , value text
    );

create type aux.aspect_internal_test_fts as
    ( name text
    , tsqu tsquery
    );

create type aux.aspect_internal_tests as
    ( tests_equality aux.aspect_internal_test_equality[]
    , tests_fts aux.aspect_internal_test_fts[]
    , combined_tsqu tsquery
    );


create type api.document_result as
    ( number integer
    , distance float4
    , recency int4
    , year int4
    , document api.document
    );

comment on type api.document_result is
    'A single result from a full text search, containing a document';
comment on column api.document_result.number is
    'Sequence number of this result';
comment on column api.document_result.distance is
    'A measure of the relevance of this result with respect to the query expression; lower values means higher relevance';
comment on column api.document_result.recency is
    'A measure for the actuality of this result. '
    'Currently the negative value of the nodeid is used as an approximation of the recency.';
comment on column api.document_result.year is
    'Year of publication, used for chronological sorting';
comment on column api.document_result.document is
    'The resulting document';


create type api.document_result_page as
    ( "offset" integer
    , has_next_page boolean
    , content api.document_result[]
    );

comment on type api.document_result_page is
    'A result page from a full text search of documents';
comment on column api.document_result_page.has_next_page is
    'Indicates whether there are more results after the current page';
comment on column api.document_result_page.content is
    'A list of document results from a full text search';


create type api.folder_count as
    ( folder_id int4
    , count integer
    );

comment on type api.folder_count is
    'Specification of the number of documents of a given set within a folder';


create type api.docset as
    ( folder_id int4
    , folder_count api.folder_count
    , id_list int4[]
    );

comment on type api.docset is
    'A set of documents as the result of e.g. a FTS query. '
    'Intended to be used with a facet or folder counting function.';
comment on column api.docset.folder_id is
    'The id of the parent folder, that was used in the query. '
    'All documents of the set belong to this folder.';
comment on column api.docset.folder_count is
    'The total count of documents in the set';
comment on column api.docset.id_list is
    'The list of documents in the set. '
    'Although this field may be queried on its own, '
    'it is more of a interim result to be consumed by a counting function.';


create type api.facet_value as
    ( value text
    , count integer
    );

comment on type api.facet_value is
    'A facet instance, i.e. the occurences of an attribute''s value.';
comment on column api.facet_value.value is
    'The value of the facet.';
comment on column api.facet_value.count is
    'The number of occurences of the facet.';


