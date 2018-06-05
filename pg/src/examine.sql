

begin;

create schema if not exists examine;


create or replace view examine.schema_types as
    select "schema", type, count (*)
    from mediatum.node
    group by "schema", type;


create or replace view examine.diss_attrs as
    select jsonb_object_keys (attrs), count (node.id)
    from node where schema = 'diss'
    group by jsonb_object_keys (attrs)
    order by count (node.id) desc;


commit;
