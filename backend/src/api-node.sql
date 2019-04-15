
-- Publicly exposed GraphQL functions
-- for generic access to nodes (that may be folders or documents)


create or replace function api.generic_node_by_id (id int4)
    returns api.generic_node as $$
    select node.id
    from entity.node
    where entity.node.id = generic_node_by_id.id
$$ language sql stable;

comment on function api.generic_node_by_id (id int4) is
    'Gets a generic node by its mediaTUM node id.';


create or replace function api.generic_node_as_document (node api.generic_node)
    returns api.document as $$
    select *
    from entity.document
    where entity.document.id = node.id
$$ language sql stable;

comment on function api.generic_node_as_document (node api.generic_node) is
    'Gets the document possibly represented by the generic node';


create or replace function api.generic_node_as_folder (node api.generic_node)
    returns api.folder as $$
    select *
    from entity.folder
    where entity.folder.id = node.id
$$ language sql stable;

comment on function api.generic_node_as_folder (node api.generic_node) is
    'Gets the folder possibly represented by the generic node';
