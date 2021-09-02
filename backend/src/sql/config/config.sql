
-- 

insert into config.application values
    -- ('hsb', array[604993]) -- 604993 is the root node
    ('hsb', array[1433087]) -- 1433087 is the "Hochschulbibliographie" node
    -- ('hsb', array[1433088, 1433089, 1515316]) -- Multiple root folders, using several years of hsb
    -- ('hsb', array[1459256]) -- A directory
    -- ('hsb', array[1459256, 1433088, 1433089, 1515316]) -- Multiple root folders, using a directory first and then several years of hsb
    -- ('hsb', array[1433088, 1433089, 1515316, 1459256]) -- Multiple root folders, using several years of hsb first, and a directory last
    -- ('hsb', '{}') -- No root folder, for testing only
;

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

insert into config.aspect_fts values
    ('title', '{"en": "Title", "de": "Titel"}'::jsonb),
    ('author', '{"en": "Author", "de": "Autor"}'::jsonb),
    ('person', '{"en": "Person", "de": "Person"}'::jsonb),
    ('keywords', '{"en": "Keywords", "de": "Stichworte"}'::jsonb),
    ('description', '{"en": "Abstract", "de": "Kurzfassung"}'::jsonb)
;

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

insert into config.masks_by_purpose values
    ('listing', '{"en": "nodesmall_en", "de": "nodesmall"}'::jsonb),
    ('details', '{"en": "nodebig_en", "de": "nodebig"}'::jsonb)
;