
-- Publicly exposed GraphQL functions
-- regarding configuration

create type api.setup as
    ( application text
    , client_name text
    , client_version text
    );
-- TODO: Hide these fields of api.setup from API at this level.

create type api.translations as
    ( en text
    , de text
    );

create type api.fts_aspect_config as
    ( aspect text
    , label api.translations
    );

create type api.facet_aspect_config as
    ( aspect text
    , label api.translations
    );

create type api.masks_purpose_config as
    ( purpose text
    , mask_names api.translations
    );

create type api.collection_page as
    ( folder_id int4
    , content api.translations
    );

create type api.setup_config as
    ( toplevel_folders int4[]
    , default_limit integer
    , max_limit integer
    , default_sorting api.fts_sorting
    , number_of_facet_values integer
    , static_fts_aspects api.fts_aspect_config[]
    , static_facet_aspects api.facet_aspect_config[]
    , masks_by_purpose api.masks_purpose_config[]
    , collection_pages api.collection_page[]
    );

create or replace function api.setup
    ( "application" text
    , client_name text
    , client_version text
    )
    returns api.setup
    as $$
    begin
	    case "application"
	    	when 'hsb' then 
		    	return ("application", client_name, client_version);
		    else
		    	return null;
	    end case;
    end;
$$ language plpgsql strict stable;

create or replace function api.setup_config
    ( setup api.setup
    )
    returns api.setup_config
    as $$
    select
        (select (toplevel_folder_ids)
            from config.application
            where name = setup.application
        ) as toplevel_folders,
    	10 as default_limit,
    	500 as max_limit,
    	'by_rank'::api.fts_sorting as default_sorting,
        20 as number_of_facet_values,
        (select array(
            select (
                aspect,
                (label->>'en', label->>'de')::api.translations
            )::api.fts_aspect_config
            from config.aspect_fts
        )) as static_fts_aspects,
        (select array(
            select (
                aspect,
                (label->>'en', label->>'de')::api.translations
            )::api.facet_aspect_config
            from config.aspect_facet
        )) as static_facet_aspects,
        (select array(
            select (
                purpose,
                (mask_names->>'en', mask_names->>'de')::api.translations
            )::api.masks_purpose_config
            from config.masks_by_purpose
        )) as static_facet_aspects,
        (select array( 
            select ( folder_id
                   , ( min(html) filter (where language = 'en')
                     , min(html) filter (where language = 'de')
                     )::api.translations
                   )::api.collection_page
            from config.collection_page
            where application = 'hsb'
            group by folder_id
        )) as collection_pages
    ;

$$ language sql stable;
