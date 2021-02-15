
-- Publicly exposed GraphQL functions
-- regarding configuration

create type api.setup as
    ( application text
    , client_name text
    , client_version text
    );
-- TODO: Hide these fields of api.setup from API at this level.

create type api.ltext as
    ( en text
    , de text
    );

create type api.fts_aspect_config as
    ( name text
    , label api.ltext
    );

create type api.facet_aspect_config as
    ( name text
    , label api.ltext
    );

create type api.setup_config as
    ( default_page_size integer
    , default_sorting api.fts_sorting
    , number_of_facet_values integer
    , static_fts_aspects api.fts_aspect_config[]
    , static_facet_aspects api.facet_aspect_config[]
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
    	10 as default_page_size,
    	'by_rank'::api.fts_sorting as default_sorting,
        20 as number_of_facet_values,
        '{}'::api.fts_aspect_config[] as static_fts_aspects,
        '{}'::api.facet_aspect_config[] as static_facet_aspects
$$ language sql stable;
