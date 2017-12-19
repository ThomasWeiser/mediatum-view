

begin;


create or replace function api.all_metadatatypes (bibtexmapping text, citeprocmapping text, find text)
    returns setof api.metadatatype as $$
    select * from entity.metadatatype
    where (all_metadatatypes.bibtexmapping is null or metadatatype.bibtexmapping = all_metadatatypes.bibtexmapping)
      and (all_metadatatypes.citeprocmapping is null or metadatatype.citeprocmapping = all_metadatatypes.citeprocmapping)
      -- TODO: Quick and dirty pattern matching ahead. Needs more elaboration:
      --       - Escape special characters (% and _ for ilike)
      --       - Handle patterns with several words (search for results containing all of them)
      --       - We may use FTS instead of pattern matching. Probably needs new index. But should be faster too.
      and (all_metadatatypes.find is null or metadatatype.longname ilike ('%' || all_metadatatypes.find || '%'))
$$ language sql stable rows 100;


create or replace function api.metadatatype_by_name (name text)
    returns api.metadatatype as $$
    select * from entity.metadatatype
    where entity.metadatatype.name = metadatatype_by_name.name
$$ language sql stable;


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


create or replace function api.metadatatype_attrs(mdt api.metadatatype, keys text[])
    returns jsonb as $$
    select aux.get_node_attrs(mdt.id, keys)
$$ language sql stable;


create or replace function api.metadatatype_system_attrs(mdt api.metadatatype, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attrs(mdt.id, keys)
$$ language sql stable;


create or replace function api.metafield_attrs(metafield api.metafield, keys text[])
    returns jsonb as $$
    select aux.get_node_attrs(metafield.id, keys)
$$ language sql stable;


create or replace function api.metafield_system_attrs(metafield api.metafield, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attrs(metafield.id, keys)
$$ language sql stable;


create or replace function api.mask_attrs(mask api.mask, keys text[])
    returns jsonb as $$
    select aux.get_node_attrs(mask.id, keys)
$$ language sql stable;


create or replace function api.mask_system_attrs(mask api.mask, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attrs(mask.id, keys)
$$ language sql stable;


create or replace function api.maskitem_attrs(maskitem api.maskitem, keys text[])
    returns jsonb as $$
    select aux.get_node_attrs(maskitem.id, keys)
$$ language sql stable;


create or replace function api.maskitem_system_attrs(maskitem api.maskitem, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attrs(maskitem.id, keys)
$$ language sql stable;


create or replace function api.metafield_metadatatype(mf api.metafield)
    returns api.metadatatype as $$
    select * from entity.metadatatype
    where metadatatype.id = mf.metadatatype_id
$$ language sql stable;


create or replace function api.metadatatype_metafields(mdt api.metadatatype, type text, find text)
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


create or replace function api.metadatatype_metafield_by_name(mdt api.metadatatype, name text)
    returns api.metafield as $$
    select * from entity.metafield
    where metafield.metadatatype_id = mdt.id
      and metafield.name = metadatatype_metafield_by_name.name
$$ language sql stable;


create or replace function api.mask_metadatatype(mask api.mask)
    returns api.metadatatype as $$
    select * from entity.metadatatype
    where metadatatype.id = mask.metadatatype_id
$$ language sql stable;


create or replace function api.metadatatype_masks(mdt api.metadatatype, masktype text, language text, is_default boolean, is_vgroup boolean, find text)
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


create or replace function api.metadatatype_mask_by_name(mdt api.metadatatype, name text)
    returns api.mask as $$
    select * from entity.mask
    where mask.metadatatype_id = mdt.id
      and mask.name = metadatatype_mask_by_name.name
$$ language sql stable;


create or replace function api.mask_maskitems(mask api.mask, type text, fieldtype text, is_required boolean, find text)
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


create or replace function api.mask_maskitem_by_name(mask api.mask, name text)
    returns api.maskitem as $$
    select * from entity.maskitem
    where maskitem.parent_id = mask.id
      and maskitem.name = mask_maskitem_by_name.name
$$ language sql stable;


create or replace function api.maskitem_subitems(parent api.maskitem, type text, is_required boolean, find text)
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


create or replace function api.maskitem_subitem_by_name(parent api.maskitem, name text)
    returns api.maskitem as $$
    select * from entity.maskitem
    where maskitem.parent_id = parent.id
      and maskitem.name = maskitem_subitem_by_name.name
$$ language sql stable;


create or replace function api.maskitem_superitem(child api.maskitem)
    returns api.maskitem as $$
    select * from entity.maskitem
    where maskitem.id = child.parent_id
$$ language sql stable;


create or replace function api.maskitem_mask(child api.maskitem)
    returns api.mask as $$
    select * from entity.mask
    where mask.id = child.parent_id
$$ language sql stable;

comment on function api.maskitem_mask(api.maskitem) is
    'Get the mask of this maskitem. Returns null is this maskitem is a subitem of another maskitem';
-- We may want a recursive query to get the mask of a nested maskitem too.


create or replace function api.maskitem_metafield(maskitem api.maskitem)
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


commit;
