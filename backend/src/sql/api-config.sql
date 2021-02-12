
-- Publicly exposed GraphQL functions
-- regarding configuration

create type api.config as
    ( "application" text
    , client text
    , client_version text
    );
   
-- comment on type api.config is E'@omit client';   
   

create type api.config_defaults as
    ( "limit" integer
    , sort_by text
    );


create or replace function api.config
    ( "application" text
    , client text default null
    , client_version text default null
    )
    returns api.config
    as $$
    begin
	    case "application"
	    	when 'hsb' then 
		    	return ("application", client, client_version);
		    else
		    	return null;
	    end case;
    end;
$$ language plpgsql stable;


create or replace function api.config_defaults
    ( config api.config
    )
    returns api.config_defaults
    as $$
    select
    	10 as "limit",
    	'rank' as sort_by
$$ language sql stable;
