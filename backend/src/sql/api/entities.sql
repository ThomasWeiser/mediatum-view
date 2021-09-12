
-- Queries to extract the publicly exposed API objects
-- from mediaTUM's generic node table.


create schema if not exists entity;


create materialized view entity.folder_node as
    select *
    from mediatum.node
    where node.type in ('collections', 'collection', 'directory')
    and node.name is not null
    and aux.is_public_today (node.id);
   
create unique index ix_folder_mode on entity.folder_node (id);   


-- TODO: We don't need to count the subfolders. We just want to know if there are any.
create or replace view entity.subfolder_count as
	select
        folder_node.id as id,
        cast (count (subfolder.id) as integer) as num_subfolder
    from entity.folder_node
    left join
	(
	    select nodemapping.nid as id
		from mediatum.nodemapping
		join entity.folder_node on folder_node.id = nodemapping.cid
	) as subfolder
	on folder_node.id = subfolder.id
    group by (folder_node.id);


create materialized view entity.folder as
    select
        node.id as id,
        case when node.type = 'collections'
            then null
            else min (to_parent.nid)
        end as parent_id,
        node.name as name,
        node.orderpos as orderpos,
        node.type in ('collections', 'collection') as is_collection,
        subfolder_count.num_subfolder > 0 as has_subfolder
    from entity.folder_node as node
    join mediatum.nodemapping as to_parent on node.id = to_parent.cid
    join mediatum.node as parent on to_parent.nid = parent.id
    join entity.subfolder_count on node.id = subfolder_count.id
    group by (node.id, node.type, node.name, node.orderpos, is_collection, subfolder_count.num_subfolder)
    order by node.orderpos;


create unique index ix_folder_id on entity.folder (id);
create index ix_folder_name on entity.folder (name);


create or replace function entity.superfolder (child api.folder)
    returns api.folder as $$
    select * from entity.folder
    where folder.id = child.parent_id
$$ language sql stable;


create materialized view entity.metadatatype as
    select
        node.id,
        node.name,
        node.attrs ->> 'longname'  as longname,
        node.attrs ->> 'datatypes'  as datatypes,
        node.attrs ->> 'description'  as description,
        node.attrs ->> 'bibtexmapping'  as bibtexmapping,
        node.attrs ->> 'citeprocmapping'  as citeprocmapping
    from mediatum.node as parent
    join mediatum.nodemapping on parent.id = nodemapping.nid
    join mediatum.node on node.id = nodemapping.cid
    where parent.type = 'metadatatypes'
      and node.type = 'metadatatype'
      -- TODO: How to deal with attribute active? It may be 0, 1.
      and node.attrs ->> 'active' = '1'
      and node.name is not null
    order by node.orderpos;


create unique index ix_metadatatype_id on entity.metadatatype (id);
create unique index ix_metadatatype_name on entity.metadatatype (name);


create materialized view entity.metafield as
    select
        node.id as id,
        metadatatype.id as metadatatype_id,
        node.name as name,
        node.orderpos as orderpos,
        -- node.attrs ->> 'defaultvalue'  as defaultvalue,
        node.attrs ->> 'description'  as description,
        node.attrs ->> 'label'  as label,
        -- node.attrs ->> 'multilang'  as multilang,
        -- node.attrs ->> 'multiple'  as multiple,
        -- node.attrs ->> 'opts'  as opts,
        -- node.attrs ->> 'required'  as required,
        node.attrs ->> 'type'  as type
        -- node.attrs ->> 'valuelist'  as valuelist,
        -- node.attrs ->> 'valuelistnum'  as valuelistnum,
        -- node.attrs ->> 'valueunit'  as valueunit
    from entity.metadatatype
    join mediatum.nodemapping on metadatatype.id = nodemapping.nid
    join mediatum.node on node.id = nodemapping.cid
    where node.type = 'metafield'
    order by node.orderpos;


create unique index ix_metafield_id on entity.metafield (id);
create index ix_metafield_metadatatype_id on entity.metafield (metadatatype_id);
create index ix_metafield_metadatatype_id_name on entity.metafield (metadatatype_id, name);


create materialized view entity.mask as
    select
        node.id as id,
        metadatatype.id as metadatatype_id,
        node.name as name,
        node.orderpos as orderpos,
        node.attrs ->> 'masktype' as masktype,
        node.attrs ->> 'language' as language,
        node.attrs ->> 'description' as description,
        node.attrs ->> 'defaultmask' = 'True' as is_default,
        case node.attrs ->> 'type'
            when 'vgroup' then true
            else
                case node.attrs ->> 'type' is null
                    when true then false
                    else null
                end
        end as is_vgroup
    from entity.metadatatype
    join mediatum.nodemapping on metadatatype.id = nodemapping.nid
    join mediatum.node on node.id = nodemapping.cid
    where node.type = 'mask'
    order by node.orderpos;


create unique index ix_mask_id on entity.mask (id);
create unique index ix_mask_metadatatype_id_name on entity.mask (metadatatype_id, name);
create index ix_mask_metadatatype_id on entity.mask (metadatatype_id);
create index ix_mask_masktype on entity.mask (masktype);


create materialized view entity.maskitem as
    select distinct
        node.id as id,
        to_parent.nid as parent_id,
        node.name as name,
        node.orderpos as orderpos,
        node.attrs ->> 'type' as type,
        (coalesce (node.attrs ->> 'fieldtype', 'standard')) as fieldtype,
        (node.attrs ->> 'width')::int4 as width,
        (node.attrs ->> 'required' = '1') as is_required
    from mediatum.node
    join mediatum.nodemapping as to_parent on node.id = to_parent.cid
    where node.type = 'maskitem'
    order by node.orderpos;


create unique index ix_maskitem_id on entity.maskitem (id);
create index ix_maskitem_parent_id on entity.maskitem (parent_id);


create materialized view entity.maskitem_recursive as
    with recursive descendant as (
        select
            node.id as id,
            mask.id as mask_id,
            mask.metadatatype_id as metadatatype_id,
            null::int4 as superitem_id,
            1 as depth,
            node.name as name,
            node.orderpos as orderpos,
            node.attrs ->> 'type' as type,
            (coalesce (node.attrs ->> 'fieldtype', 'standard')) as fieldtype,
            (node.attrs ->> 'width')::int4 as width,
            (node.attrs ->> 'required' = '1') as is_required
        from entity.mask
        join mediatum.nodemapping on mask.id = nodemapping.nid
        join mediatum.node on node.id = nodemapping.cid
        where node.type = 'maskitem'
        union ALL
        select
            node.id as id,
            parent.mask_id as mask_id,
            parent.metadatatype_id as metadatatype_id,
            parent.id as superitem_id,
            parent.depth + 1 as depth,
            node.name as name,
            node.orderpos as orderpos,
            node.attrs ->> 'type' as type,
            (coalesce (node.attrs ->> 'fieldtype', 'standard')) as fieldtype,
            (node.attrs ->> 'width')::int4 as width,
            (node.attrs ->> 'required' = '1') as is_required
        from descendant as parent
        join mediatum.nodemapping on parent.id = nodemapping.nid
        join mediatum.node on node.id = nodemapping.cid
        where node.type = 'maskitem'
    )
    select *
    from descendant
    order by orderpos;


create materialized view entity.mapping as
    select
        node.id as id,
        node.name as name,
        node.orderpos as orderpos,
        node.attrs ->> 'mappingtype' as type,
        node.attrs ->> 'description' as description
    from mediatum.node as parent
    join mediatum.nodemapping on parent.id = nodemapping.nid
    join mediatum.node on node.id = nodemapping.cid
    where parent.type = 'mappings'
      and node.type = 'mapping'
      -- TODO: How to deal with attribute active? It may be 0, 1 or absent.
      and (not node.attrs ? 'active' or node.attrs ->> 'active' = '1')
      and node.name is not null
    order by node.orderpos;


create materialized view entity.mappingfield as
    select
        node.id as id,
        mapping.id as mapping_id,
        node.name as name,
        node.orderpos as orderpos,
        node.attrs ->> 'description'  as description,
        node.attrs ->> 'mandatory' = 'True' as is_mandatory
    from entity.mapping
    join mediatum.nodemapping on mapping.id = nodemapping.nid
    join mediatum.node on node.id = nodemapping.cid
    where node.type = 'mappingfield'
    order by node.orderpos;


create or replace view entity.document as
    select
        node.id,
        node.type :: text,
        node.schema :: text,
        node.name :: text,
        node.orderpos,
        node.attrs
    from mediatum.node
    where node.schema is not null
      and not aux.nodetype_is_container (node.type)
      and aux.is_public_today (node.id);


-- View on node for getting documents, where we don't need to check `not nodetype.is_container`
create or replace view entity.node as
    select
        node.id,
        node.type,
        node.schema,
        node.name,
        node.orderpos,
        node.attrs
    from mediatum.node
    where aux.is_public_today (node.id);


create or replace view entity.document_mask_fields as
    select
        document.id as document_id,
        document.type as document_type,
        document.schema as document_schema,
        document.name as document_name,
        document.orderpos as document_orderpos,
        document.attrs as document_attrs,
        mask.id as mask_id,
        mask.name as mask_name,
        maskitem.id as maskitem_id,
        maskitem.name as maskitem_name,
        maskitem.orderpos as maskitem_orderpos,
        maskitem.width as maskitem_width,
        metafield.id as metafield_id,
        metafield.name as metafield_name,
        document.attrs->metafield.name #>> '{}' as value
    from entity.document
    join entity.metadatatype on entity.metadatatype.name = entity.document.schema
    join entity.mask on entity.mask.metadatatype_id = entity.metadatatype.id
    join entity.maskitem on entity.maskitem.parent_id = entity.mask.id
    join mediatum.nodemapping on mediatum.nodemapping.nid = entity.maskitem.id
    join entity.metafield on entity.metafield.id = mediatum.nodemapping.cid;
    

/* Currently not used.
   Tests didn't show performance gains when using this materialized view.

create materialized view entity.metadatatype_mask_fields as
    select
        metadatatype.name as metadatatype_name,
        mask.name as mask_name,
        maskitem.name as maskitem_name,
        metafield.name as metafield_name
    from entity.metadatatype
    join entity.mask on entity.mask.metadatatype_id = entity.metadatatype.id
    join entity.maskitem on entity.maskitem.parent_id = entity.mask.id
    join mediatum.nodemapping on mediatum.nodemapping.nid = entity.maskitem.id
    join entity.metafield on entity.metafield.id = mediatum.nodemapping.cid;

create index ix_metadatatype_mask_fields on entity.metadatatype_mask_fields
    (metadatatype_name, mask_name, maskitem_name);
*/    
   

create or replace view entity.document_mask_value_object as
    select
        document_id,
        mask_name,
        jsonb_object_agg (
            metafield_name,
            jsonb_build_object (
                'name',
                maskitem_name,
                'orderpos',
                maskitem_orderpos,
                'width',
                maskitem_width,
                'value',
                value
            )
        ) as values
    from entity.document_mask_fields
    group by document_id, mask_name;


create or replace view entity.document_mask_value_list as
    select
        document_id,
        mask_name,
        jsonb_agg (
            jsonb_build_object (
                'field',
                metafield_name,
                'name',
                maskitem_name,
                'width',
                maskitem_width,
                'value',
                value
            )
        ) as values
    from (select * from entity.document_mask_fields order by maskitem_orderpos) as q
    group by document_id, mask_name;

------------------------------------------------------------------

-- Possible index on table noderelation

/* Add this index only if proven useful.
   Up to now we don't have a query that would profit from it.
   Caution: I've experienced some performance loss _with_ the index,
            when checking for node lineage using a `exists` sub-query on noderelation.
            Without this index the planner uses the pkey index and is a bit faster.

-- We have already a multi-column index (nid, cid, distance) from primary key.
-- For enumerating parents we add a multi-column index (cid, nid) here.
create index if not exists ix_mediatum_noderelation_cid_nid
    on noderelation
 using btree (cid, nid);
*/

