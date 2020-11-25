

drop table if exists preprocess.aspect_def cascade;
create table preprocess.aspect_def (
    name text primary key,
    keys text[],
    split_at_semicolon boolean,
    normalize_year boolean
);
insert into preprocess.aspect_def values
    ('type', array['type'], false, false),
    ('origin', array['origin'], false, false),
    ('subject', array['subject'], true, false),
    ('subject2', array['subject2'], true, false),
    ('title', array['title', 'title-translated'], false, false),
    ('author', array['author', 'author.fullname_comma'], true, false),
    ('person', array['author', 'author.fullname_comma', 'advisor', 'referee'], true, false),
    ('keywords', array['keywords', 'keywords-translated'], true, false),
    ('description', array['description', 'description-translated'], false, false),
    ('year', array['year'], false, true)
;


drop table if exists preprocess.aspect cascade;
create table preprocess.aspect (
	nid int4 references mediatum.node(id) on delete cascade,
    name text,
    values text[] not null,
	tsvec tsvector not null,
    primary key (nid, name)
);


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


drop view if exists preprocess.aspect_view;

create view preprocess.aspect_view as
    select
        document.id as nid,
        aspect_def.name as name,
        preprocess.some_attributes_as_array(document.attrs, aspect_def.keys, aspect_def.split_at_semicolon, aspect_def.normalize_year) as values,
        preprocess.some_attributes_as_tsvector(document.attrs, aspect_def.keys, aspect_def.split_at_semicolon, aspect_def.normalize_year) as tsvec
    from mediatum.node as document, preprocess.aspect_def
    where document.schema is not null
        and not aux.nodetype_is_container (document.type)
;


------------------------------------

delete from preprocess.aspect;   

-- Suppress notices about "Word is too long to be indexed. Words longer than 2047 characters are ignored."
-- We don't mind that such long lexemes don't get indexed.
set session client_min_messages to warning;

insert into preprocess.aspect (nid, name, values, tsvec)
    select nid, name, values, tsvec
    from preprocess.aspect_view
    -- where nid > 601000 and nid < 602000 -- For testing: process some well-known documents only
    -- where nid > 1515316
    -- limit 44000 -- For testing: process only a smaller number of rows
;

-- Reset message level to default
set session client_min_messages to notice;

------------------------------------

create index if not exists aspect_rum_tsvector_ops
    on preprocess.aspect
 using rum (tsvec rum_tsvector_ops)
;

create index if not exists aspect_rum_tsvector_addon_ops
    on preprocess.aspect
 using rum (tsvec rum_tsvector_addon_ops, name)
  with (attach ='name', to = 'tsvec')
 ;

-- TODO: Create index on (name, values), using gin utilizing extension btree_gin
--       See https://stackoverflow.com/q/31945601

analyze preprocess.aspect;
