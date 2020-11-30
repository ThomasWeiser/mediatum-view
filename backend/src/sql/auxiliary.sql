
-- Some auxiliary functions, that live in their own schema.

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


create or replace function aux.node_self_and_children (parent_node_id int4)
    returns setof int4 as $$
        begin
	      return next parent_node_id;
	      return query
	      	select cid from mediatum.nodemapping where nid = parent_node_id;
        end;
$$ language plpgsql stable;


create or replace function aux.node_children (parent_node_id int4)
    returns setof int4 as $$
        begin
	      return query
	      	select cid from mediatum.nodemapping where nid = parent_node_id;
        end;
$$ language plpgsql stable;


create or replace function aux.custom_to_tsquery (query text)
    returns tsquery as $$
    begin
        -- Check the availability of the function "websearch_to_tsquery",
        -- which has been introduced in PostgreSQL 11.
        -- If running an older version of PostgreSQL we fall back to "plainto_tsquery".
        if exists(select from pg_proc where proname = 'websearch_to_tsquery')
        then
            return websearch_to_tsquery ('english_german'::regconfig, query);
        else
            return plainto_tsquery ('english_german'::regconfig, query);
        end if;
    end;
$$ language plpgsql immutable;


create or replace function aux.ts_headline_options (highlight_all boolean)
    returns text as $$
    declare delimiter text := 'StartSel=<mediatum:fts>, StopSel=</mediatum:fts>';
    begin
        if highlight_all then
            return delimiter || ', HighlightAll=true';
        else
            return delimiter || ', HighlightAll=false';
        end if;
    end;
$$ language plpgsql immutable;


-- Set weight(s) on a tsquery, like PostgreSQL's setweight does on a tsvector.
-- Taken from: https://stackoverflow.com/a/45338769
create function aux.setweight (query tsquery, weights text)
    returns tsquery as $$
    select regexp_replace(
                query::text, 
                '(?<=[^ !])'':?(\*?)A?B?C?D?',
                ''':\1'||weights, 
                'g'
            )::tsquery;
$$ language sql strict immutable;


create function aux.convert_to_or_query (query tsquery)
    returns tsquery as $$
    select regexp_replace(
                querytree(query), 
                -- TODO: Make sure we don't replace a quoted ampersand
                ' & ', 
                ' | ', 
                'g'
            )::tsquery;
$$ language sql strict immutable;


create aggregate aux.tsquery_and_agg(tsquery) (
  sfunc = tsquery_and,
  stype = tsquery
);


create function aux.internalize_aspect_tests (array_of_tests api.aspect_test[])
    returns aux.aspect_internal_tests as $$
        select
            ( coalesce(
                (array_agg((test.name, test.value)::aux.aspect_internal_test_equality) 
                    filter (where operator = 'equality')
                ),
                '{}'
              )::aux.aspect_internal_test_equality[]
            , coalesce(
                (array_agg((test.name, aux.custom_to_tsquery (test.value))::aux.aspect_internal_test_fts)
                    filter (where operator = 'fts')
                ),
                '{}'
              )::aux.aspect_internal_test_fts[]
            , (aux.tsquery_and_agg(aux.custom_to_tsquery (test.value)))::tsquery
            )::aux.aspect_internal_tests
        from
            unnest (array_of_tests) as test
$$ language sql strict immutable;


create function aux.check_aspect_internal_tests (document_id int4, internal_tests aux.aspect_internal_tests)
    returns boolean as $$
    declare test_equality aux.aspect_internal_test_equality;
    declare test_fts aux.aspect_internal_test_fts;
    begin
        foreach test_equality in array internal_tests.tests_equality
        loop
            perform 1
                from preprocess.aspect
                where  aspect.nid = document_id
                    and (aspect.name = test_equality.name)
                    and (array[test_equality.value] <@ aspect.values);
            if not found then
                return false;
            end if;
        end loop;
        foreach test_fts in array internal_tests.tests_fts
        loop
            perform 1
                from preprocess.aspect
                where  aspect.nid = document_id
                    and (aspect.name = test_fts.name)
                    and aspect.tsvec @@ test_fts.tsqu;
            if not found then
                return false;
            end if;
        end loop;
        return true;
    end;
$$ language plpgsql stable strict parallel safe;


create function aux.aspect_tests (document_id int4, array_of_tests api.aspect_test[])
    -- TODO Remove when docset functions are adapted to check_aspect_internal_tests
    returns boolean as $$
    declare test api.aspect_test;
    begin
        foreach test in array array_of_tests
        loop
        	case test.operator
                when 'equality' then
	        		perform 1
						from preprocess.aspect
						where  aspect.nid = document_id
							and (aspect.name = test.name)
							and (array[test.value] <@ aspect.values);
                    if not found then
                        return false;
                    end if;
                when 'fts' then
	        		perform 1
						from preprocess.aspect
						where  aspect.nid = document_id
							and (aspect.name = test.name)
							and aspect.tsvec @@ aux.custom_to_tsquery (test.value);
                    if not found then
                        return false;
                    end if;
           end case;
        end loop;
        return true;
    end;
$$ language plpgsql stable strict parallel safe;


-- Strip whitescape from either end of the string.
-- And replace NULL with the empty string.
create or replace function aux.normalize_facet_value (value text)
    returns text as $$
        select trim (E' \f\n\r\t' from (coalesce (value, '')))
$$ language sql immutable;


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


create or replace function aux.jsonb_test_list (obj jsonb, tests api.attribute_test[])
    returns boolean as $$
    declare test api.attribute_test;
    declare key_value text;
    begin
        foreach test in array tests
        loop
            key_value := obj ->> test.key;
            case test.operator
                when 'equality' then
                    if key_value is null then
                        return false; 
                    end if;
                    if key_value != test.value then
                        return false;
                    end if;
                when 'equalitywithblanknull' then
                    if test.value = '' then
                       if aux.normalize_facet_value(key_value) != '' then
                           return false;
                        end if;
                    elsif key_value is null  or key_value != test.value then
                        return false;
                    end if;
                when 'ilike' then
                    if key_value is null then
                        return false; 
                    end if;
                    if not (key_value ilike test.value
                            or (test.extra is not null and key_value ilike test.extra)
                           )
                    then
                        return false;
                    end if;
                when 'simplefts' then
                    if key_value is null then
                        return false; 
                    end if;
                    if not to_tsvector('english_german', key_value) @@ aux.custom_to_tsquery(test.value) then
                        return false;
                    end if;
                when 'daterange' then
                    if key_value is null then 
                        return false;
                    end if;
                    if left(key_value, length (test.value)) < test.value then
                        return false;
                    end if;
                    if left(key_value, length (test.extra)) > test.extra then
                        return false;
                    end if;
            end case;
        end loop;
        return true;
    end;
$$ language plpgsql immutable;


create or replace function aux.get_document_attributes (document api.document, keys text[])
    returns jsonb as $$
    select aux.jsonb_filter (document.attrs, keys)
$$ language sql stable;


create or replace function aux.get_document_attribute (document api.document, key text)
    returns jsonb as $$
    select  document.attrs->key
$$ language sql stable;


create or replace function aux.get_node_attributes (nodeId int4, keys text[])
    returns jsonb as $$
    select aux.jsonb_filter (node.attrs, keys)
      from mediatum.node
     where node.id = nodeId
$$ language sql stable;


create or replace function aux.get_node_attribute (nodeId int4, key text)
    returns jsonb as $$
    select  node.attrs->key
      from mediatum.node
     where node.id = nodeId
$$ language sql stable;


create or replace function aux.get_node_attribute_text (nodeId int4, key text)
    returns text as $$
    select  (node.attrs->key) #>> '{}'
      from mediatum.node
     where node.id = nodeId
$$ language sql stable;


create or replace function aux.get_node_system_attributes (nodeId int4, keys text[])
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


create or replace function aux.nodetype_is_container (nodetype1 varchar)
    returns boolean as $$
    select exists
        ( select 1
          from mediatum.nodetype
          where nodetype.name = nodetype1
          and nodetype.is_container
        )
$$ language sql stable;
