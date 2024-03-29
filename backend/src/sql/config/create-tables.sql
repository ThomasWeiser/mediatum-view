

create schema if not exists config;

create table if not exists config.application (
    name text primary key,
    toplevel_folder_ids int4[]
);

create table if not exists config.aspect_def (
    name text primary key,
    keys text[],
    split_at_semicolon boolean,
    normalize_year boolean
);

create table if not exists config.aspect_fts (
    aspect text primary key references config.aspect_def (name) on delete cascade,
    label jsonb not null
);

create table if not exists config.aspect_facet (
    aspect text primary key references config.aspect_def (name) on delete cascade,
    label jsonb not null
);

create table if not exists config.masks_by_purpose (
    purpose text primary key not null,
    mask_names jsonb not null
);

create table if not exists config.collection_page (
    application text,
    folder_id int4,
    language text,
    html text not null,
    primary key (application, folder_id, language)
);
