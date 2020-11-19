

drop table if exists preprocess.aspect;  

create table preprocess.aspect (
	nid serial references mediatum.node(id) on delete cascade,
    name text,
    values text[],
	tsvec tsvector null,
    primary key (nid, name)
);


create or replace function preprocess.some_attributes_as_array (attrs jsonb, keys text[])
    returns text[] as $$
	select array(
        select left(value, 1048000) from jsonb_each_text(attrs) where key = any (keys)
    )
$$ language sql immutable;


create or replace function preprocess.some_attributes_as_text (attrs jsonb, keys text[])
    returns text as $$
	select
        array_to_string(preprocess.some_attributes_as_array(attrs, keys), ' ')
$$ language sql immutable;


create or replace function preprocess.some_attributes_as_tsvector (attrs jsonb, keys text[])
    returns tsvector as $$
	select
        to_tsvector('english_german', preprocess.some_attributes_as_text(attrs, keys))
$$ language sql immutable;


create or replace procedure preprocess.add_document_aspect (document mediatum.node, name text, keys text[])
    as $$
        insert into preprocess.aspect (nid, name, values, tsvec)
            select
                document.id,
                name,
                preprocess.some_attributes_as_array(document.attrs, keys) as values,
                preprocess.some_attributes_as_tsvector(document.attrs, keys) as tsvec
        ;
$$ language sql;

create or replace function preprocess.add_document_aspects (document mediatum.node)
    returns void as $$
        call preprocess.add_document_aspect(document, 'title', array['title', 'title-translated']);
$$ language sql volatile;


create or replace procedure preprocess.populate_aspect_table ()
    as $$
        select preprocess.add_document_aspects(node::mediatum.node)
            from mediatum.node
            where node.schema is not null
                and not aux.nodetype_is_container (node.type)
        limit 1000 -- For testing the code we may just process a small fraction of the data
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
