
-- Publicly exposed GraphQL functions
-- demonstrating mutations


begin;


create or replace function api.set_document_attribute (id int4, key text, value text)
    returns api.document as $$

    update entity.document
    set attrs = jsonb_set (attrs, array[key], to_jsonb(value), true)
    where document.id = set_document_attribute.id
    returning *

$$ language sql volatile;

comment on function api.set_document_attribute (id int4, key text, value text) is
    'Sets a node attribute of a document given by id.';


commit;
