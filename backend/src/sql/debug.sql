
-- Functions used during development, that link the public GraphQL types with the mediaTUM node table.
-- Useful when examining the structure of the node table.


create or replace function debug.all_nodes (
        type text,
        schema text,
        name text,
        is_subnode boolean,
        find text,
        find_in_attrs text,
        attr text,
        attr_value text,
        attr_find text
    )
    returns setof debug.mediatum_node as $$
    select id, type, schema, name, orderpos, fulltext, subnode
    from mediatum.node
    where (all_nodes.type is null or node.type = all_nodes.type)
      and (all_nodes.schema is null or node.schema = all_nodes.schema)
      and (all_nodes.name is null or node.name = all_nodes.name)
      and (all_nodes.is_subnode is null or node.subnode = all_nodes.is_subnode)
      and (all_nodes.find is null or node.name ilike ('%' || all_nodes.find || '%'))
      and (all_nodes.find_in_attrs is null or node.attrs::text ilike ('%' || all_nodes.find_in_attrs || '%'))
      and (all_nodes.attr is null or node.attrs ? all_nodes.attr and (
                (all_nodes.attr_value is null or node.attrs ->> all_nodes.attr = all_nodes.attr_value)
            and (all_nodes.attr_find  is null or node.attrs ->> all_nodes.attr ilike ('%' || all_nodes.attr_find || '%'))
          ))
$$ language sql stable;


create or replace function debug.node_by_id (id int4)
    returns debug.mediatum_node as $$
    select id, type, schema, name, orderpos, fulltext, subnode
    from mediatum.node
    where node.id = node_by_id.id
$$ language sql stable;


create or replace function debug.mediatum_node_attributes (node debug.mediatum_node, keys text[])
    returns jsonb as $$
    select aux.get_node_attributes (node.id, keys)
$$ language sql stable;


create or replace function debug.mediatum_node_system_attributes (node debug.mediatum_node, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attributes (node.id, keys)
$$ language sql stable;


create or replace function debug.mediatum_node_parent_nodes (
        child_node debug.mediatum_node,
        type text,
        schema text,
        name text,
        is_subnode boolean,
        find text,
        find_in_attrs text,
        attr text,
        attr_value text,
        attr_find text
    )
    returns setof debug.mediatum_node as $$
    select id, type, schema, name, orderpos, fulltext, subnode
    from mediatum.nodemapping, mediatum.node
    where nodemapping.cid = child_node.id
      and node.id = nodemapping.nid
      and (mediatum_node_parent_nodes.type is null or node.type = mediatum_node_parent_nodes.type)
      and (mediatum_node_parent_nodes.schema is null or node.schema = mediatum_node_parent_nodes.schema)
      and (mediatum_node_parent_nodes.name is null or node.name = mediatum_node_parent_nodes.name)
      and (mediatum_node_parent_nodes.is_subnode is null or node.subnode = mediatum_node_parent_nodes.is_subnode)
      and (mediatum_node_parent_nodes.find is null or node.name ilike ('%' || mediatum_node_parent_nodes.find || '%'))
      and (mediatum_node_parent_nodes.find_in_attrs is null or node.attrs::text ilike ('%' || mediatum_node_parent_nodes.find_in_attrs || '%'))
      and (mediatum_node_parent_nodes.attr is null or node.attrs ? mediatum_node_parent_nodes.attr and (
                (mediatum_node_parent_nodes.attr_value is null or node.attrs ->> mediatum_node_parent_nodes.attr = mediatum_node_parent_nodes.attr_value)
            and (mediatum_node_parent_nodes.attr_find  is null or node.attrs ->> mediatum_node_parent_nodes.attr ilike ('%' || mediatum_node_parent_nodes.attr_find || '%'))
          ))
$$ language sql stable;


create or replace function debug.mediatum_node_child_nodes (
        parent_node debug.mediatum_node,
        type text,
        schema text,
        name text,
        is_subnode boolean,
        find text,
        find_in_attrs text,
        attr text,
        attr_value text,
        attr_find text
    )
    returns setof debug.mediatum_node as $$
    select id, type, schema, name, orderpos, fulltext, subnode
    from mediatum.nodemapping, mediatum.node
    where nodemapping.nid = parent_node.id
      and node.id = nodemapping.cid
      and (mediatum_node_child_nodes.type is null or node.type = mediatum_node_child_nodes.type)
      and (mediatum_node_child_nodes.schema is null or node.schema = mediatum_node_child_nodes.schema)
      and (mediatum_node_child_nodes.name is null or node.name = mediatum_node_child_nodes.name)
      and (mediatum_node_child_nodes.is_subnode is null or node.subnode = mediatum_node_child_nodes.is_subnode)
      and (mediatum_node_child_nodes.find is null or node.name ilike ('%' || mediatum_node_child_nodes.find || '%'))
      and (mediatum_node_child_nodes.find_in_attrs is null or node.attrs::text ilike ('%' || mediatum_node_child_nodes.find_in_attrs || '%'))
      and (mediatum_node_child_nodes.attr is null or node.attrs ? mediatum_node_child_nodes.attr and (
                (mediatum_node_child_nodes.attr_value is null or node.attrs ->> mediatum_node_child_nodes.attr = mediatum_node_child_nodes.attr_value)
            and (mediatum_node_child_nodes.attr_find  is null or node.attrs ->> mediatum_node_child_nodes.attr ilike ('%' || mediatum_node_child_nodes.attr_find || '%'))
          ))
$$ language sql stable;


/* Would like to put the following debug functions into schema `debug`.
   But input data type and function must live in same schema.
   https://www.graphile.org/postgraphile/computed-columns/
   So these functions must not be made accessible in production
   as it reveals private parts of the `node` table!

   Maybe we should remove these functions altogether!
   Don't think they are that useful anyway!.
*/
create or replace function api.metadatatype_node (metadatatype api.metadatatype)
    returns debug.mediatum_node as $$
    select id, type, schema, name, orderpos, fulltext, subnode
    from mediatum.node
    where node.id = metadatatype.id
$$ language sql stable;


create or replace function api.metafield_node (metafield api.metafield)
    returns debug.mediatum_node as $$
    select id, type, schema, name, orderpos, fulltext, subnode
    from mediatum.node
    where node.id = metafield.id
$$ language sql stable;


create or replace function api.mask_node (mask api.mask)
    returns debug.mediatum_node as $$
    select id, type, schema, name, orderpos, fulltext, subnode
    from mediatum.node
    where node.id = mask.id
$$ language sql stable;


create or replace function api.maskitem_node (maskitem api.maskitem)
    returns debug.mediatum_node as $$
    select id, type, schema, name, orderpos, fulltext, subnode
    from mediatum.node
    where node.id = maskitem.id
$$ language sql stable;


create or replace function api.mapping_node (mapping api.mapping)
    returns debug.mediatum_node as $$
    select id, type, schema, name, orderpos, fulltext, subnode
    from mediatum.node
    where node.id = mapping.id
$$ language sql stable;


create or replace function api.mappingfield_node (mappingfield api.mappingfield)
    returns debug.mediatum_node as $$
    select id, type, schema, name, orderpos, fulltext, subnode
    from mediatum.node
    where node.id = mappingfield.id
$$ language sql stable;


create or replace function api.document_node (document api.document)
    returns debug.mediatum_node as $$
    select id, type, schema, name, orderpos, fulltext, subnode
    from mediatum.node
    where node.id = document.id
$$ language sql stable;


create or replace function debug.mediatum_node_as_metadatatype (node debug.mediatum_node)
    returns api.metadatatype as $$
    select * from entity.metadatatype
    where entity.metadatatype.id = node.id
$$ language sql stable;


create or replace function debug.mediatum_node_as_metafield (node debug.mediatum_node)
    returns api.metafield as $$
    select * from entity.metafield
    where entity.metafield.id = node.id
$$ language sql stable;


create or replace function debug.mediatum_node_as_mask (node debug.mediatum_node)
    returns api.mask as $$
    select * from entity.mask
    where entity.mask.id = node.id
$$ language sql stable;


create or replace function debug.mediatum_node_as_maskitem (node debug.mediatum_node)
    returns api.maskitem as $$
    select * from entity.maskitem
    where entity.maskitem.id = node.id
$$ language sql stable;


create or replace function debug.mediatum_node_as_mapping (node debug.mediatum_node)
    returns api.mapping as $$
    select * from entity.mapping
    where entity.mapping.id = node.id
$$ language sql stable;


create or replace function debug.mediatum_node_as_mappingfield (node debug.mediatum_node)
    returns api.mappingfield as $$
    select * from entity.mappingfield
    where entity.mappingfield.id = node.id
$$ language sql stable;


create or replace function debug.mediatum_node_as_document (node debug.mediatum_node)
    returns api.document as $$
    select * from entity.document
    where entity.document.id = node.id
$$ language sql stable;
