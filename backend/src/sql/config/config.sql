
-- 

insert into config.application 
    (name, toplevel_folder_ids)
values
    -- ('hsb', array[604993]) -- 604993 is the root node
    ('hsb', array[1433087]) -- 1433087 is the "Hochschulbibliographie" node
    -- ('hsb', array[1433088, 1433089, 1515316]) -- Multiple root folders, using several years of hsb
    -- ('hsb', array[1459256]) -- A directory
    -- ('hsb', array[1459256, 1433088, 1433089, 1515316]) -- Multiple root folders, using a directory first and then several years of hsb
    -- ('hsb', array[1433088, 1433089, 1515316, 1459256]) -- Multiple root folders, using several years of hsb first, and a directory last
    -- ('hsb', '{}') -- No root folder, for testing only
    on conflict (name) do update set toplevel_folder_ids = excluded.toplevel_folder_ids
;

delete from config.masks_by_purpose;
delete from config.aspect_facet;
delete from config.aspect_fts;
delete from config.aspect_def;
delete from config.frontpage;

insert into config.masks_by_purpose
    (purpose, mask_names)
values
    ('listing', '{"en": "nodesmall_en", "de": "nodesmall"}'::jsonb),
    ('details', '{"en": "nodebig_en", "de": "nodebig"}'::jsonb)
;

insert into config.aspect_def
    (name, keys, split_at_semicolon, normalize_year)
values
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

insert into config.aspect_fts
    (aspect, label)
values
    ('title', '{"en": "Title", "de": "Titel"}'::jsonb),
    ('author', '{"en": "Author", "de": "Autor"}'::jsonb),
    ('person', '{"en": "Person", "de": "Person"}'::jsonb),
    ('keywords', '{"en": "Keywords", "de": "Stichworte"}'::jsonb),
    ('description', '{"en": "Abstract", "de": "Kurzfassung"}'::jsonb)
;

insert into config.aspect_facet
    (aspect, label)
values
    ('type', '{"en": "Document type", "de": "Dokumenttyp"}'::jsonb),
    ('subject', '{"en": "Subject group", "de": "Fachgebiet"}'::jsonb),
    ('subject2', '{"en": "TUM classification", "de": "TU-Systematik"}'::jsonb),
    ('origin', '{"en": "Institution", "de": "Institution"}'::jsonb),
    ('author', '{"en": "Author", "de": "Autor"}'::jsonb),
    ('person', '{"en": "Person", "de": "Person"}'::jsonb),
    ('keywords', '{"en": "Keywords", "de": "Stichworte"}'::jsonb),
    ('year', '{"en": "Year", "de": "Jahr"}'::jsonb)
;

\set hsb_frontpage_en_html `cat src/content/hsb-frontpage-en.html`
\set hsb_frontpage_de_html `cat src/content/hsb-frontpage-de.html`

insert into config.frontpage
    (application, language, html)
values
    ('hsb', 'en', :'hsb_frontpage_en_html'),
    ('hsb', 'de', :'hsb_frontpage_de_html')
;
