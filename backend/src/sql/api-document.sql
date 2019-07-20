
-- Publicly exposed GraphQL functions
-- regarding document objects.


create or replace function aux.all_documents_limited
    ( folder_id int4
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    , "limit" integer
    )
    returns setof api.document
    as $$
    select
        (document.id, document.type, document.schema, document.name, document.orderpos, document.attrs)::api.document as document
    from entity.document
    join aux.node_lineage on document.id = node_lineage.descendant
    where folder_id = node_lineage.ancestor
    and (all_documents_limited.type is null or document.type = all_documents_limited.type)
    and (all_documents_limited.name is null or document.name = all_documents_limited.name)
    and (attribute_tests is null or aux.jsonb_test_list (document.attrs, attribute_tests))
    order by document.id
    limit "limit"
    ;
$$ language sql stable rows 100;


create or replace function aux.all_documents_paginated
    ( folder_id int4
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    , "limit" integer
    , "offset" integer
    )
    returns table
        ( document api.document
        , distance float4
        , number integer
        , has_next_page boolean
        )
    as $$
        begin return query
            select f
                , 0.0::float4
                , (row_number () over ())::integer as number
                , (count(*) over ()) > "limit" + "offset"
            from aux.all_documents_limited
                ( folder_id
                , type
                , name
                , attribute_tests
                , "limit" + "offset" + 1
                ) as f
            limit "limit"
            offset "offset";
        end;
$$ language plpgsql stable parallel safe rows 100;


create or replace function api.all_documents_page
    ( folder_id int4
    , type text
    , name text
    , attribute_tests api.attribute_test[]
    , "limit" integer
    , "offset" integer
    )
    returns api.document_result_page as $$

        with search_result as (
                select *
                from aux.all_documents_paginated
                    ( folder_id
                    , type, name
                    , attribute_tests
                    , "limit", "offset"
                    )
            )
        select
            "offset",
            coalesce(
                (select every(has_next_page) from search_result), false
            ) as has_next_page,
            array (
            select row(number, distance, document)::api.document_result
                from search_result
            ) as content
        ;
$$ language sql stable parallel safe;




/*
Actually, we would like to declare that the first parameter is required.

This can be done by annotating the function as `strict` and using default arguments for the
optional parameters. See https://github.com/graphile/postgraphile/issues/438

Unfortunately, this leads to inefficient query execution.
See http://www.postgresonline.com/journal/archives/163-STRICT-on-SQL-Function-Breaks-In-lining-Gotcha.html

We tested with PostgreSQL version 9.6.5
Maybe a later of PostgreSQL version will fix this inefficiency.
Then we may want to amend the API here.

create or replace function api.all_documents (folder_id int4, type text='', name text='')
    returns setof api.document as $$
    select document.*
    from entity.document
    join aux.node_lineage on document.id = node_lineage.descendant
    where folder_id = node_lineage.ancestor
    and (all_documents.type = '' or document.type = all_documents.type)
    and (all_documents.name = '' or document.name = all_documents.name);
$$ language sql stable strict rows 10000;
*/


comment on function api.all_documents_page (folder_id int4, type text, name text, attribute_tests api.attribute_test[], "limit" integer, "offset" integer) is
    'Reads and enables pagination through all documents in a folder, optionally filtered by type and name and a list of attribute tests';


create or replace function api.document_by_id (id int4)
    returns api.document as $$
    select *
    from entity.document
    where document.id = document_by_id.id
$$ language sql stable;

comment on function api.document_by_id (id int4) is
    'Gets a document by its mediaTUM node id.';


create or replace function api.document_attributes (document api.document, keys text[])
    returns jsonb as $$
    select aux.get_document_attributes (document, keys)
$$ language sql stable;

comment on function api.document_attributes (document api.document, keys text[]) is
    'Gets the node attributes of this document as a JSON value, optionally filtered by a list of keys.';


create or replace function api.document_system_attributes (document api.document, keys text[])
    returns jsonb as $$
    select aux.get_node_system_attributes (document.id, keys)
$$ language sql stable;

comment on function api.document_system_attributes (document api.document, keys text[]) is
    'Gets the node system attributes of this document as a JSON value, optionally filtered by a list of keys.';


create or replace function api.document_metadatatype (document api.document)
    returns api.metadatatype as $$
    select api.metadatatype_by_name (document.schema)
$$ language sql stable;

comment on function api.document_metadatatype (document api.document) is
    'Gets the meta data type of this document.';


create or replace function api.metadatatype_documents (mdt api.metadatatype, type text, name text)
    returns setof api.document as $$
    select document.*
    from entity.node as document
    where document.schema = mdt.name
      and (metadatatype_documents.type is null or document.type = metadatatype_documents.type)
      and (metadatatype_documents.name is null or document.name = metadatatype_documents.name)
$$ language sql stable;

comment on function api.metadatatype_documents (mdt api.metadatatype, type text, name text) is
    'Reads and enables pagination through all documents having this meta data type, optionally filtered by type and name.';


create or replace function api.document_values_by_mask (document api.document, mask_name text)
    returns jsonb as $$
    select v.values
    from entity.document_mask_value_list as v
    where v.document_id = document_values_by_mask.document.id
      and v.mask_name = document_values_by_mask.mask_name
$$ language sql stable parallel safe;

comment on function api.document_values_by_mask (document api.document, mask_name text) is
    'Gets the meta field values of this document as a JSON value, selected by a named mask.';