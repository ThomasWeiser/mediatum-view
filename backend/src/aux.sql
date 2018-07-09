
-- Some auxiliary functions, that live in their own schema.

begin;


create schema if not exists aux;


create or replace view aux.node_lineage as
    select distinct
        nid as ancestor,
        cid as descendant
    from mediatum.noderelation;


create or replace function aux.test_node_lineage (ancestor int4, descendant int4)
    returns boolean as $$
        begin
            perform
                from mediatum.noderelation
                where noderelation.cid = descendant
                  and noderelation.nid = ancestor
                limit 1;
            return found;
        end;
$$ language plpgsql stable;


create or replace function aux.jsonb_filter (obj jsonb, keys text[])
    returns jsonb as $$
    declare result jsonb := '{}'::jsonb;
    declare key text;
    begin
        if keys is null then
            result := obj;
        else
            foreach key in array keys
            loop
                if obj ? key then
                    result := jsonb_set (result, array[key], obj -> key);
                end if;
            end loop;
        end if;
        return result;
    end;
$$ language plpgsql immutable;


create or replace function aux.get_node_attrs (nodeId int4, keys text[])
    returns jsonb as $$
    select aux.jsonb_filter (node.attrs, keys)
      from mediatum.node
     where node.id = nodeId
$$ language sql stable;


create or replace function aux.get_node_attr (nodeId int4, key text)
    returns jsonb as $$
    select  node.attrs->key
      from mediatum.node
     where node.id = nodeId
$$ language sql stable;


create or replace function aux.get_node_system_attrs (nodeId int4, keys text[])
    returns jsonb as $$
    select aux.jsonb_filter (node.system_attrs, keys)
      from mediatum.node
     where node.id = nodeId
$$ language sql stable;


create or replace function aux.is_public_today (nodeId int4)
    returns boolean as $$
    select mediatum.has_read_access_to_node (nodeId,
            _group_ids => '{2}', -- Group ID 2 is 'Gast' here. May need a more generic lookup.
            ipaddr => inet '0.0.0.0',
            _date => date 'today'
        )
$$ language sql stable;


commit;
