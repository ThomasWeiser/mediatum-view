

drop table if exists preprocess.aspect;  

create table preprocess.aspect (
	nid serial references mediatum.node(id) on delete cascade,
    name text,
    values text[],
	tsvec tsvector null,
    primary key (nid, name)
);


create or replace function preprocess.flatten_array (nested_array anyarray)
    returns anyarray as $$
	select array_agg(u)
      from unnest() as u
$$ language sql immutable;


create or replace function preprocess.array_unique_stable (input_array anyarray)
    -- https://dba.stackexchange.com/a/211502
    returns anyarray as $$
    select array_agg(element order by index)
    from (
        select distinct on (element) element,index
        from unnest(input_array) with ordinality as p(element,index)
        order by element,index
    ) sub
$$ language sql immutable strict;


create or replace function preprocess.some_attributes_as_array (attrs jsonb, keys text[], split_at_semicolon boolean default false)
    returns text[] as $$
    select preprocess.array_unique_stable(
        case when split_at_semicolon then
            (   select array(
                    select left(u, 1048000)
                    from jsonb_each_text(attrs), unnest (regexp_split_to_array(value, ';')) as u
                    where key = any (keys)
                )
            )
        else
            (   select array(
                    -- Refactor string-normalizing function
                    select left(value, 1048000)
                    from jsonb_each_text(attrs) 
                    where key = any (keys)
                )
            )
        end
    )
$$ language sql immutable;



create or replace function preprocess.some_attributes_as_text (attrs jsonb, keys text[], split_at_semicolon boolean default false)
    returns text as $$
	select
        array_to_string(preprocess.some_attributes_as_array(attrs, keys, split_at_semicolon), ' ')
$$ language sql immutable;


create or replace function preprocess.some_attributes_as_tsvector (attrs jsonb, keys text[], split_at_semicolon boolean default false)
    returns tsvector as $$
	select
        to_tsvector('english_german', preprocess.some_attributes_as_text(attrs, keys, split_at_semicolon))
$$ language sql immutable;


create or replace procedure preprocess.add_document_aspect (document mediatum.node, name text, keys text[], split_at_semicolon boolean default false)
    as $$
        insert into preprocess.aspect (nid, name, values, tsvec)
            select
                document.id,
                name,
                preprocess.some_attributes_as_array(document.attrs, keys, split_at_semicolon) as values,
                preprocess.some_attributes_as_tsvector(document.attrs, keys, split_at_semicolon) as tsvec
        ;
$$ language sql;

create or replace function preprocess.add_document_aspects (document mediatum.node)
    returns void as $$
        call preprocess.add_document_aspect(document, 'type', array['type'], false);
        call preprocess.add_document_aspect(document, 'origin', array['origin'], false);
        call preprocess.add_document_aspect(document, 'subject', array['subject'], false);
        call preprocess.add_document_aspect(document, 'subject2', array['subject2'], true);
        call preprocess.add_document_aspect(document, 'title', array['title', 'title-translated'], false);
        call preprocess.add_document_aspect(document, 'author', array['author', 'author.fullname_comma'], true);
        call preprocess.add_document_aspect(document, 'person', array['author', 'author.fullname_comma', 'advisor', 'referee'], true);
        call preprocess.add_document_aspect(document, 'keywords', array['keywords', 'keywords-translated'], true);
        call preprocess.add_document_aspect(document, 'description', array['description', 'description-translated'], false);
$$ language sql volatile;


create or replace procedure preprocess.populate_aspect_table ()
    as $$
        select preprocess.add_document_aspects(node::mediatum.node)
            from mediatum.node
            where node.schema is not null
                and not aux.nodetype_is_container (node.type)
        limit 10000 -- For testing the code we may just process a small fraction of the data
        ;
$$ language sql;


-- Suppress notices about "Word is too long to be indexed. Words longer than 2047 characters are ignored."
-- We don't mind that such long lexemes don't get indexed.
set session client_min_messages to warning;

delete from preprocess.aspect;   
call preprocess.populate_aspect_table ();

-- Reset message level to default
set session client_min_messages to notice;

create index if not exists aspect_rum_tsvector_ops
    on preprocess.aspect
 using rum (tsvec rum_tsvector_ops)
;

create index if not exists aspect_rum_tsvector_addon_ops
    on preprocess.aspect
 using rum (tsvec rum_tsvector_addon_ops, name)
  with (attach ='name', to = 'tsvec')
 ;
