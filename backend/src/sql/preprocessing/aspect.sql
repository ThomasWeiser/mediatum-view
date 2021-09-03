
create or replace function preprocess.prepare_values (values_array text[])
    -- 1. Eliminate duplicate values with stable sort order
    --    (cf https://dba.stackexchange.com/a/211502).
    -- 2. Remove null values (which come from empty attrs values)
    -- 3. If there are no values, return an array containing the empty string
    --    (which denotes the special value "not specified").
    returns text[] as $$
    select coalesce (array_agg(element order by index), array[''])
    from (
        select distinct on (element) element,index
        from unnest(values_array) with ordinality as p(element,index)
        where element is not null
        order by element,index
    ) sub
$$ language sql immutable strict;


create or replace function preprocess.normalize_value (value text, normalize_year boolean)
    returns text as $$
    select
        left (
            substring(
                substring (value,
                    case when normalize_year then
                        '\d{4}'
                    else
                        '.*'
                    end
                )
                , '(\S.*\S)'
            )
            , 1048000
        )
        
$$ language sql immutable strict;


create or replace function preprocess.some_attributes_as_array (attrs jsonb, keys text[], split_at_semicolon boolean, normalize_year boolean)
    returns text[] as $$
    select preprocess.prepare_values(
        case when split_at_semicolon then
            array(
                select preprocess.normalize_value (unnested_value, normalize_year)
                from jsonb_each_text(attrs), unnest (regexp_split_to_array(value, ';')) as unnested_value
                where key = any (keys)
            )
        else
            array(
                select preprocess.normalize_value (value, normalize_year)
                from jsonb_each_text(attrs) 
                where key = any (keys)
            )
        end
    )
$$ language sql immutable strict;


create or replace function preprocess.some_attributes_as_text (attrs jsonb, keys text[], split_at_semicolon boolean, normalize_year boolean)
    returns text as $$
	select
        array_to_string(preprocess.some_attributes_as_array(attrs, keys, split_at_semicolon, normalize_year), ' ')
$$ language sql immutable strict;


create or replace function preprocess.some_attributes_as_tsvector (attrs jsonb, keys text[], split_at_semicolon boolean, normalize_year boolean)
    returns tsvector as $$
	select
        to_tsvector('english_german', preprocess.some_attributes_as_text(attrs, keys, split_at_semicolon, normalize_year))
$$ language sql immutable strict;


create or replace view preprocess.aspect_as_view as
    select
        document.id as nid,
        aspect_def.name as name,
        preprocess.some_attributes_as_array(document.attrs, aspect_def.keys, aspect_def.split_at_semicolon, aspect_def.normalize_year) as values,
        preprocess.some_attributes_as_tsvector(document.attrs, aspect_def.keys, aspect_def.split_at_semicolon, aspect_def.normalize_year) as tsvec
    from
        mediatum.node as document,
        mediatum.nodetype,
        config.aspect_def
    where
        document.schema is not null
        and document.type = nodetype.name
        and not nodetype.is_container
;


create or replace function preprocess.update_aspect_on_node_upsert()
    returns trigger
    as $$
    begin
        insert into preprocess.aspect (nid, name, values, tsvec)
            select nid, name, values, tsvec
            from preprocess.aspect_as_view
            where aspect_as_view.nid = new.id
            and (nid >= current_setting('mediatum.preprocessing_min_node_id', true)::int) is not false
            and (nid <= current_setting('mediatum.preprocessing_max_node_id', true)::int) is not false
            on conflict on constraint aspect_pkey
            do update set 
                values = excluded.values,
                tsvec = excluded.tsvec
        ;

        return new;
    end;
$$ language plpgsql;

