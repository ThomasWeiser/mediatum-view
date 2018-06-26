
# Database structure

## Types

```
api.metadatatype
api.metafield
api.mask
api.maskitem
api.maskitem_reachable
api.mapping
api.mappingfield
api.document
```

## Views

```
materialized view entity.metadatatype
materialized view entity.metafield
materialized view entity.mask
materialized view entity.maskitem
materialized view entity.maskitem_recursive
materialized view entity.mapping
materialized view entity.mappingfield
view entity.document
view entity.node
view entity.document_mask_fields
view entity.document_mask_value_object
view entity.document_mask_value_list
```

## API Metadata

```
function api.all_metadatatypes (bibtexmapping text, citeprocmapping text, find text)
function api.metadatatype_by_name (name text)
function api.all_metafields (name text, type text, find text)
function api.all_masks (name text, masktype text, language text, is_default boolean, is_vgroup boolean, find text)
function api.all_maskitems (name text, type text, fieldtype text, is_required boolean, find text)
function api.all_maskitem_reachable (name text, type text, fieldtype text, is_required boolean, find text)
function api.all_mappings (type text, find text)
function api.mapping_by_name (name text)
function api.all_mappingfields (name text, is_mandatory boolean, find text)
function api.metadatatype_attrs(mdt api.metadatatype, keys text[])
function api.metadatatype_system_attrs(mdt api.metadatatype, keys text[])
function api.metafield_attrs(metafield api.metafield, keys text[])
function api.metafield_system_attrs(metafield api.metafield, keys text[])
function api.mask_attrs(mask api.mask, keys text[])
function api.mask_system_attrs(mask api.mask, keys text[])
function api.maskitem_attrs(maskitem api.maskitem, keys text[])
function api.maskitem_system_attrs(maskitem api.maskitem, keys text[])
function api.metafield_metadatatype(mf api.metafield)
function api.mapping_attrs(mapping api.mapping, keys text[])
function api.mapping_system_attrs(mapping api.mapping, keys text[])
function api.mappingfield_attrs(mappingfield api.mappingfield, keys text[])
function api.mappingfield_system_attrs(mappingfield api.mappingfield, keys text[])
function api.metadatatype_metafields(mdt api.metadatatype, type text, find text)
function api.metadatatype_metafield_by_name(mdt api.metadatatype, name text)
function api.mask_metadatatype(mask api.mask)
function api.metadatatype_masks(mdt api.metadatatype, masktype text, language text, is_default boolean, is_vgroup boolean, find text)
function api.metadatatype_mask_by_name(mdt api.metadatatype, name text)
function api.mask_maskitems(mask api.mask, type text, fieldtype text, is_required boolean, find text)
function api.mask_maskitem_by_name(mask api.mask, name text)
function api.maskitem_subitems(parent api.maskitem, type text, is_required boolean, find text)
function api.maskitem_subitem_by_name(parent api.maskitem, name text)
function api.maskitem_superitem(child api.maskitem)
function api.maskitem_mask(child api.maskitem)
function api.maskitem_metafield(maskitem api.maskitem)
function api.maskitem_mappingfield(maskitem api.maskitem)
function api.maskitem_template(maskitem api.maskitem)
function api.mappingfield_mapping(mf api.mappingfield)
function api.mapping_mappingfields(m api.mapping, is_mandatory boolean, find text)
function api.mapping_mappingfield_by_name(m api.mapping, name text)
function api.mask_exportmapping(mask api.mask)
```

## API Document

```
function api.all_documents (type text, name text)
function api.document_by_id (id int4)
function api.document_attrs(document api.document, keys text[])
function api.document_system_attrs(document api.document, keys text[])
function api.document_metadatatype(document api.document)
function api.metadatatype_documents(mdt api.metadatatype, type text, name text)
function api.document_values_by_mask(document api.document, mask_name text)
function api.simple_search (text text, languages text [], domains text [], "limit" integer)
function api.simple_search_unranked (text text, languages text [function api.author_search (text text) 
```
