
-- 

drop schema if exists config cascade;
create schema if not exists config;


create table config.aspect_def (
    name text primary key,
    keys text[],
    split_at_semicolon boolean,
    normalize_year boolean
);

insert into config.aspect_def values
    ('type', array['type'], false, false),
    ('origin', array['origin'], false, false),
    ('subject', array['subject'], true, false),
    ('subject2', array['subject2'], true, false),
    ('title', array['title', 'title-translated', 'title-contrib'], false, false),
    ('author', array['author', 'author-contrib', 'author.fullname_comma'], true, false),
    ('person', array['author', 'author-contrib', 'author.fullname_comma', 'advisor', 'referee'], true, false),
    ('keywords', array['keywords', 'keywords-translated'], true, false),
    ('description', array['description', 'description-translated'], false, false),
    ('year', array['year', 'year-accepted'], false, true)
;


create table config.aspect_fts (
    aspect text primary key references config.aspect_def (name),
    label jsonb not null
);

insert into config.aspect_fts values
    ('title', '{"en": "Title", "de": "Titel"}'::jsonb),
    ('author', '{"en": "Author", "de": "Autor"}'::jsonb),
    ('person', '{"en": "Person", "de": "Person"}'::jsonb),
    ('keywords', '{"en": "Keywords", "de": "Stichworte"}'::jsonb),
    ('description', '{"en": "Abstract", "de": "Kurzfassung"}'::jsonb)
;


create table config.aspect_facet (
    aspect text primary key references config.aspect_def (name),
    label jsonb not null
);

insert into config.aspect_facet values
    ('type', '{"en": "Document type", "de": "Dokumenttyp"}'::jsonb),
    ('subject', '{"en": "Document type", "de": "Dokumenttyp"}'::jsonb),
    ('subject2', '{"en": "TUM classification", "de": "TU-Systematik"}'::jsonb),
    ('origin', '{"en": "Institution", "de": "Institution"}'::jsonb),
    ('author', '{"en": "Author", "de": "Autor"}'::jsonb),
    ('person', '{"en": "Person", "de": "Person"}'::jsonb),
    ('keywords', '{"en": "Keywords", "de": "Stichworte"}'::jsonb),
    ('year', '{"en": "Year", "de": "Jahr"}'::jsonb)
;


create table config.masks_by_purpose (
    purpose text not null,
    mask_names jsonb not null
);

insert into config.masks_by_purpose values
    ('listing', '{"en": "nodesmall_en", "de": "nodesmall"}'::jsonb),
    ('details', '{"en": "nodebig_en", "de": "nodebig"}'::jsonb)
;