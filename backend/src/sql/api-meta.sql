
-- Publicly exposed GraphQL functions
-- regarding meta objects.


/*
TODO: Quick and dirty pattern matching ahead. Needs more elaboration:
  - Escape special characters (% and _ for ilike)
  - Handle patterns with several words (search for results containing all of them)
  - We may use FTS instead of pattern matching. Probably needs new index. But should be faster too.
*/      


create or replace function api.all_metadatatypes (bibtexmapping text, citeprocmapping text, find text)
    returns setof api.metadatatype as $$
    select * from entity.metadatatype
    where (all_metadatatypes.bibtexmapping is null or metadatatype.bibtexmapping = all_metadatatypes.bibtexmapping)
      and (all_metadatatypes.citeprocmapping is null or metadatatype.citeprocmapping = all_metadatatypes.citeprocmapping)
      and (all_metadatatypes.find is null or metadatatype.longname ilike ('%' || all_metadatatypes.find || '%'))
$$ language sql stable rows 100;

comment on function api.all_metadatatypes (bibtexmapping text, citeprocmapping text, find text) is
    'Reads and enables pagination through all meta data types, optionally filtered by bibtexmapping and citeprocmapping, and searchable by longname.';


create or replace function api.metadatatype_by_name (name text)
    returns api.metadatatype as $$
    select * from entity.metadatatype
    where entity.metadatatype.name = metadatatype_by_name.name
$$ language sql stable;

comment on function api.metadatatype_by_name (name text) is
    'Gets a meta data type by its name.';


create or replace function api.all_metafields (name text, type text, find text)
    returns setof api.metafield as $$
    select * from entity.metafield
    where (all_metafields.name is null or metafield.name = all_metafields.name)
      and (all_metafields.type is null or metafield.type = all_metafields.type)
      and (all_metafields.find is null
            or metafield.name ilike ('%' || all_metafields.find || '%')
            or metafield.label ilike ('%' || all_metafields.find || '%')
            or metafield.description ilike ('%' || all_metafields.find || '%')
          )
    order by metafield.orderpos
$$ language sql stable rows 3000;

comment on function api.all_metafields (name text, type text, find text) is
    'Reads and enables pagination through all meta fields, optionally filtered by name and type, and searchable by name, label and description.';


create or replace function api.all_masks (name text, masktype text, language text, is_default boolean, is_vgroup boolean, find text)
    returns setof api.mask as $$
    select * from entity.mask
    where (all_masks.name is null or mask.name = all_masks.name)
      and (all_masks.masktype is null or mask.masktype = all_masks.masktype)
      and (all_masks.language is null or mask.language = all_masks.language)
      and (all_masks.is_default is null or mask.is_default = all_masks.is_default)
      and (all_masks.is_vgroup is null or mask.is_vgroup = all_masks.is_vgroup)
      and (all_masks.find is null
            or mask.name ilike ('%' || all_masks.find || '%')
            or mask.description ilike ('%' || all_masks.find || '%')
          )
    order by mask.orderpos
$$ language sql stable rows 800;

comment on function api.all_masks (name text, masktype text, language text, is_default boolean, is_vgroup boolean, find text) is
    'Reads and enables pagination through all masks, optionally filtered by name, masktype, language, is_default, is_vgroup, and searchable by name and description.';


create or replace function api.all_maskitems (name text, type text, fieldtype text, is_required boolean, find text)
    returns setof api.maskitem as $$
    select * from entity.maskitem
    where (all_maskitems.name is null or maskitem.name = all_maskitems.name)
      and (all_maskitems.type is null or maskitem.type = all_maskitems.type)
    and (all_maskitems.fieldtype is null or maskitem.fieldtype = all_maskitems.fieldtype)
      and (all_maskitems.is_required is null or maskitem.is_required = all_maskitems.is_required)
      and (all_maskitems.find is null
            or maskitem.name ilike ('%' || all_maskitems.find || '%')
          )
    order by maskitem.orderpos
$$ language sql stable rows 800;

comment on function api.all_maskitems (name text, type text, fieldtype text, is_required boolean, find text) is
    'Reads and enables pagination through all mask items, optionally filtered by name, type, fieldtype and is_required, and searchable by name.';


create or replace function api.all_maskitem_reachable (name text, type text, fieldtype text, is_required boolean, find text)
    returns setof api.maskitem_reachable as $$
    select * from entity.maskitem_recursive
    where (all_maskitem_reachable.name is null or maskitem_recursive.name = all_maskitem_reachable.name)
      and (all_maskitem_reachable.type is null or maskitem_recursive.type = all_maskitem_reachable.type)
    and (all_maskitem_reachable.fieldtype is null or maskitem_recursive.fieldtype = all_maskitem_reachable.fieldtype)
      and (all_maskitem_reachable.is_required is null or maskitem_recursive.is_required = all_maskitem_reachable.is_required)
      and (all_maskitem_reachable.find is null
            or maskitem_recursive.name ilike ('%' || all_maskitem_reachable.find || '%')
          )
    order by maskitem_recursive.orderpos
$$ language sql stable rows 800;

comment on function api.all_maskitem_reachable (name text, type text, fieldtype text, is_required boolean, find text) is
    'Reads and enables pagination through all mask items (alternative implementation), optionally filtered by name, type, fieldtype and is_required, and searchable by name.';


create or replace function api.all_mappings (type text, find text)
    returns setof api.mapping as $$
    select * from entity.mapping
    where (all_mappings.type is null or mapping.type = all_mappings.type)
      and (all_mappings.find is null
            or mapping.name ilike ('%' || all_mappings.find || '%')
            or mapping.description ilike ('%' || all_mappings.find || '%')
          )
    order by mapping.orderpos
$$ language sql stable rows 40;

comment on function api.all_mappings (type text, find text) is
    'Reads and enables pagination through all mappings, optionally filtered by type, and searchable by name and description.';


create or replace function api.mapping_by_name (name text)
    returns api.mapping as $$
    select * from entity.mapping
    where entity.mapping.name = mapping_by_name.name
$$ language sql stable;

comment on function api.mapping_by_name (name text) is
    'Gets a mapping by its name.';


create or replace function api.all_mappingfields (name text, is_mandatory boolean, find text)
    returns setof api.mappingfield as $$
    select * from entity.mappingfield
    where (all_mappingfields.name is null or mappingfield.name = all_mappingfields.name)
      and (all_mappingfields.is_mandatory is null or mappingfield.is_mandatory = all_mappingfields.is_mandatory)
      and (all_mappingfields.find is null
            or mappingfield.name ilike ('%' || all_mappingfields.find || '%')
            or mappingfield.description ilike ('%' || all_mappingfields.find || '%')
          )
    order by mappingfield.orderpos
$$ language sql stable rows 400;

comment on function api.all_mappingfields (name text, is_mandatory boolean, find text) is
    'Reads and enables pagination through all mapping fields, optionally filtered by name and is_mandatory, and searchable by name and description.';


create or replace function api.metadatatype_attributes (mdt api.metadatatype, keys text[])
    returns jsonb as $$
    select aux.get_node_attributes (mdt.id, keys)
$$ language sql stable;

comment on function api.metadatatype_attributes (mdt api.metadatatype, keys text[]) is
    'Gets the node attributes of this meta data type as a JSON value, optionally filtered by a list of keys.';


create or replace function api.metadatatype_system_attributes (mdt api.metadatatype, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attributes (mdt.id, keys)
$$ language sql stable;

comment on function api.metadatatype_system_attributes (mdt api.metadatatype, keys text[]) is
    'Gets the node system attributes of this meta data type as a JSON value, optionally filtered by a list of keys.';


create or replace function api.metafield_attributes (metafield api.metafield, keys text[])
    returns jsonb as $$
    select aux.get_node_attributes (metafield.id, keys)
$$ language sql stable;

comment on function api.metafield_attributes (metafield api.metafield, keys text[]) is
    'Gets the node attributes of this meta field as a JSON value, optionally filtered by a list of keys.';


create or replace function api.metafield_system_attributes (metafield api.metafield, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attributes (metafield.id, keys)
$$ language sql stable;

comment on function api.metafield_system_attributes (metafield api.metafield, keys text[]) is
    'Gets the node system attributes of this meta field as a JSON value, optionally filtered by a list of keys.';


create or replace function api.mask_attributes (mask api.mask, keys text[])
    returns jsonb as $$
    select aux.get_node_attributes (mask.id, keys)
$$ language sql stable;

comment on function api.mask_attributes (mask api.mask, keys text[]) is
    'Gets the node attributes of this mask as a JSON value, optionally filtered by a list of keys.';


create or replace function api.mask_system_attributes (mask api.mask, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attributes (mask.id, keys)
$$ language sql stable;

comment on function api.mask_system_attributes (mask api.mask, keys text[]) is
    'Gets the node system attributes of this mask as a JSON value, optionally filtered by a list of keys.';


create or replace function api.maskitem_attributes (maskitem api.maskitem, keys text[])
    returns jsonb as $$
    select aux.get_node_attributes (maskitem.id, keys)
$$ language sql stable;

comment on function api.maskitem_attributes (maskitem api.maskitem, keys text[]) is
    'Gets the node attributes of this mask item as a JSON value, optionally filtered by a list of keys.';


create or replace function api.maskitem_system_attributes (maskitem api.maskitem, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attributes (maskitem.id, keys)
$$ language sql stable;

comment on function api.maskitem_system_attributes (maskitem api.maskitem, keys text[]) is
    'Gets the node system attributes of this mask item as a JSON value, optionally filtered by a list of keys.';


create or replace function api.metafield_metadatatype (mf api.metafield)
    returns api.metadatatype as $$
    select * from entity.metadatatype
    where metadatatype.id = mf.metadatatype_id
$$ language sql stable;

comment on function api.metafield_metadatatype (mf api.metafield) is
    'Gets the meta data type associated with this meta field.';


create or replace function api.mapping_attributes (mapping api.mapping, keys text[])
    returns jsonb as $$
    select aux.get_node_attributes(mapping.id, keys)
$$ language sql stable;

comment on function api.mapping_attributes (mapping api.mapping, keys text[]) is
    'Gets the node attributes of this mapping as a JSON value, optionally filtered by a list of keys.';


create or replace function api.mapping_system_attributes (mapping api.mapping, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attributes(mapping.id, keys)
$$ language sql stable;

comment on function api.mapping_system_attributes (mapping api.mapping, keys text[]) is
    'Gets the node system attributes of this mapping as a JSON value, optionally filtered by a list of keys.';


create or replace function api.mappingfield_attributes (mappingfield api.mappingfield, keys text[])
    returns jsonb as $$
    select aux.get_node_attributes(mappingfield.id, keys)
$$ language sql stable;

comment on function api.mappingfield_attributes (mappingfield api.mappingfield, keys text[]) is
    'Gets the node attributes of this mapping field as a JSON value, optionally filtered by a list of keys.';


create or replace function api.mappingfield_system_attributes (mappingfield api.mappingfield, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attributes(mappingfield.id, keys)
$$ language sql stable;

comment on function api.mappingfield_system_attributes (mappingfield api.mappingfield, keys text[]) is
    'Gets the node system attributes of this mapping field as a JSON value, optionally filtered by a list of keys.';


create or replace function api.metadatatype_metafields (mdt api.metadatatype, type text, find text)
    returns setof api.metafield as $$
    select * from entity.metafield
    where metafield.metadatatype_id = mdt.id
      and (metadatatype_metafields.type is null or metafield.type = metadatatype_metafields.type)
      and (metadatatype_metafields.find is null
            or metafield.name ilike ('%' || metadatatype_metafields.find || '%')
            or metafield.label ilike ('%' || metadatatype_metafields.find || '%')
            or metafield.description ilike ('%' || metadatatype_metafields.find || '%')
          )
    order by metafield.orderpos
$$ language sql stable rows 80;

comment on function api.metadatatype_metafields (mdt api.metadatatype, type text, find text) is
    'Reads and enables pagination through all meta fields of this meta data type, optionally filtered by type, and searchable by name, label and description.';


create or replace function api.metadatatype_metafield_by_name (mdt api.metadatatype, name text)
    returns api.metafield as $$
    select * from entity.metafield
    where metafield.metadatatype_id = mdt.id
      and metafield.name = metadatatype_metafield_by_name.name
$$ language sql stable;

comment on function api.metadatatype_metafield_by_name (mdt api.metadatatype, name text) is
    'Gets a meta field of this meta data type by name.';


create or replace function api.mask_metadatatype (mask api.mask)
    returns api.metadatatype as $$
    select * from entity.metadatatype
    where metadatatype.id = mask.metadatatype_id
$$ language sql stable;

comment on function api.mask_metadatatype (mask api.mask) is
    'Gets the meta data type of this mask.';


create or replace function api.metadatatype_masks (mdt api.metadatatype, masktype text, language text, is_default boolean, is_vgroup boolean, find text)
    returns setof api.mask as $$
    select * from entity.mask
    where mask.metadatatype_id = mdt.id
      and (metadatatype_masks.masktype is null or mask.masktype = metadatatype_masks.masktype)
      and (metadatatype_masks.language is null or mask.language = metadatatype_masks.language)
      and (metadatatype_masks.is_default is null or mask.is_default = metadatatype_masks.is_default)
      and (metadatatype_masks.is_vgroup is null or mask.is_vgroup = metadatatype_masks.is_vgroup)
      and (metadatatype_masks.find is null
            or mask.name ilike ('%' || metadatatype_masks.find || '%')
            or mask.description ilike ('%' || metadatatype_masks.find || '%')
          )
    order by mask.orderpos
$$ language sql stable rows 80;

comment on function api.metadatatype_masks (mdt api.metadatatype, masktype text, language text, is_default boolean, is_vgroup boolean, find text) is
    'Reads and enables pagination through all masks of this meta data type, optionally filtered by type, language, is_default, is_vgroup, and searchable by name and description.';


create or replace function api.metadatatype_mask_by_name (mdt api.metadatatype, name text)
    returns api.mask as $$
    select * from entity.mask
    where mask.metadatatype_id = mdt.id
      and mask.name = metadatatype_mask_by_name.name
$$ language sql stable;

comment on function api.metadatatype_mask_by_name (mdt api.metadatatype, name text) is
    'Gets a mask of this meta data type by name.';


create or replace function api.mask_maskitems (mask api.mask, type text, fieldtype text, is_required boolean, find text)
    returns setof api.maskitem as $$
    select * from entity.maskitem
    where maskitem.parent_id = mask.id
      and (mask_maskitems.type is null or maskitem.type = mask_maskitems.type)
      and (mask_maskitems.fieldtype is null or maskitem.fieldtype = mask_maskitems.fieldtype)
      and (mask_maskitems.is_required is null or maskitem.is_required = mask_maskitems.is_required)
      and (mask_maskitems.find is null
            or maskitem.name ilike ('%' || mask_maskitems.find || '%')
          )
    order by maskitem.orderpos
$$ language sql stable rows 80;

comment on function api.mask_maskitems (mask api.mask, type text, fieldtype text, is_required boolean, find text) is
    'Reads and enables pagination through all items this mask, optionally filtered by type, fieldtype and is_required, and searchable by name.';


create or replace function api.mask_maskitem_by_name (mask api.mask, name text)
    returns api.maskitem as $$
    select * from entity.maskitem
    where maskitem.parent_id = mask.id
      and maskitem.name = mask_maskitem_by_name.name
$$ language sql stable;

comment on function api.mask_maskitem_by_name (mask api.mask, name text) is
    'Gets an item of this mask by name.';


create or replace function api.maskitem_subitems (parent api.maskitem, type text, is_required boolean, find text)
    returns setof api.maskitem as $$
    select * from entity.maskitem
    where maskitem.parent_id = parent.id
      and (maskitem_subitems.type is null or maskitem.type = maskitem_subitems.type)
      and (maskitem_subitems.is_required is null or maskitem.is_required = maskitem_subitems.is_required)
      and (maskitem_subitems.find is null
            or maskitem.name ilike ('%' || maskitem_subitems.find || '%')
          )
    order by maskitem.orderpos
$$ language sql stable rows 10;

comment on function api.maskitem_subitems (parent api.maskitem, type text, is_required boolean, find text) is
    'Reads and enables pagination through all sub-items of this mask item, optionally filtered by type and is_required, and searchable by name.';


create or replace function api.maskitem_subitem_by_name (parent api.maskitem, name text)
    returns api.maskitem as $$
    select * from entity.maskitem
    where maskitem.parent_id = parent.id
      and maskitem.name = maskitem_subitem_by_name.name
$$ language sql stable;

comment on function api.maskitem_subitem_by_name (parent api.maskitem, name text) is
    'Gets a sub-item of this mask item by name.';


create or replace function api.maskitem_superitem (child api.maskitem)
    returns api.maskitem as $$
    select * from entity.maskitem
    where maskitem.id = child.parent_id
$$ language sql stable;

comment on function api.maskitem_superitem (child api.maskitem) is
    'Gets the super-item of this mask item. Returns null if this item is a direct child of a mask.';


create or replace function api.maskitem_mask (child api.maskitem)
    returns api.mask as $$
    select * from entity.mask
    where mask.id = child.parent_id
$$ language sql stable;

comment on function api.maskitem_mask (api.maskitem) is
    'Gets the mask of this maskitem. Returns null is this maskitem is a subitem of another maskitem.';
-- We may want a recursive query to get the mask of a nested maskitem too.


create or replace function api.maskitem_metafield (maskitem api.maskitem)
    returns api.metafield as $$
    declare result api.metafield;
    begin
        case maskitem.fieldtype
            when 'standard' then
                select metafield.*
                into result
                from mediatum.nodemapping
                join entity.metafield on metafield.id = nodemapping.cid
                where maskitem.id = nodemapping.nid;
            else
                select metafield.*
                into result
                from mediatum.node
                join entity.metafield on (node.attrs->>'attribute')::int4 = metafield.id
                where maskitem.id = node.id;
        end case;
        return result;
    end;
$$ language plpgsql stable;

comment on function api.maskitem_metafield (maskitem api.maskitem) is
    'Gets the meta field associated with this mask item.';


create or replace function api.maskitem_mappingfield(maskitem api.maskitem)
    returns api.mappingfield as $$
    declare result api.mappingfield;
    begin
        case maskitem.fieldtype
            when 'mapping' then
                select mappingfield.*
                into result
                from mediatum.node
                join entity.mappingfield
                  on (node.attrs->>'mappingfield' <> 'None') and
                     (node.attrs->>'mappingfield')::int4 = mappingfield.id
                where maskitem.id = node.id;
            else
                result := null;
        end case;
        return result;
    end;
$$ language plpgsql stable;

comment on function api.maskitem_mappingfield(maskitem api.maskitem) is
    'Gets the mapping field associated with this mask item. Returns null if there is no such mapping field.';


create or replace function api.maskitem_template(maskitem api.maskitem)
    returns text as $$
    declare result text;
    begin
        case maskitem.fieldtype
            when 'attribute' then
                select node.attrs->>'mappingfield'
                into result
                from mediatum.node
                where maskitem.id = node.id;
            else
                result := null;
        end case;
        return result;
    end;
$$ language plpgsql stable;

comment on function api.maskitem_template(maskitem api.maskitem) is
    'Gets the template string of mask item. Returns null if there is no such template.';


create or replace function api.mappingfield_mapping(mf api.mappingfield)
    returns api.mapping as $$
    select * from entity.mapping
    where mapping.id = mf.mapping_id
$$ language sql stable;

comment on function api.mappingfield_mapping(mf api.mappingfield) is
    'Gets the mapping of this mapping field.';


create or replace function api.mapping_mappingfields(m api.mapping, is_mandatory boolean, find text)
    returns setof api.mappingfield as $$
    select * from entity.mappingfield
    where mappingfield.mapping_id = m.id
      and (mapping_mappingfields.is_mandatory is null or mappingfield.is_mandatory = mapping_mappingfields.is_mandatory)
      and (mapping_mappingfields.find is null
            or mappingfield.name ilike ('%' || mapping_mappingfields.find || '%')
            or mappingfield.description ilike ('%' || mapping_mappingfields.find || '%')
          )
$$ language sql stable rows 80;

comment on function api.mapping_mappingfields(m api.mapping, is_mandatory boolean, find text) is
    'Reads and enables pagination through all mapping fields of this mapping, optionally filtered by is_mandatory, and searchable by name and description.';


create or replace function api.mapping_mappingfield_by_name(m api.mapping, name text)
    returns api.mappingfield as $$
    select * from entity.mappingfield
    where mappingfield.mapping_id = m.id
      and mappingfield.name = mapping_mappingfield_by_name.name
$$ language sql stable;

comment on function api.mapping_mappingfield_by_name(m api.mapping, name text) is
    'Gets a mapping field of this mapping by name.';


create or replace function api.mask_exportmapping(mask api.mask)
    returns api.mapping as $$
    select mapping.*
    from mediatum.node
    join entity.mapping on mapping.id = (node.attrs ->> 'exportmapping')::int4
    where node.id = mask.id
$$ language sql stable;

comment on function api.mask_exportmapping(mask api.mask) is
    'Gets the export mapping of this mask. Returns null if there is no such mapping.';
