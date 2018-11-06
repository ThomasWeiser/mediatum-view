
-- Publicly exposed GraphQL functions
-- demonstrating mutations


begin;


create or replace function api.update_document_attribute (id int4, key text, value text)
    returns api.document as $$

    -- Currently we allow updating already existing attributes only.
    -- We return null if the id doesn't refer to a document or if the key doesn't exist.
    -- We should probably have some descriptive return type to indicate the outcome precisely.
    update entity.document
    set attrs = jsonb_set (attrs, array[key], to_jsonb(value), false)
    where document.id = update_document_attribute.id
    and attrs ? key 
    returning *

$$ language sql volatile;

comment on function api.update_document_attribute (id int4, key text, value text) is
    'Updates an existing attribute of the document with the given id.';


commit;
