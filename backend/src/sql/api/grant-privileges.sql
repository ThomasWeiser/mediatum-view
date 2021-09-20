
-- 

-- We define a user with restricted privileges for API access via postgraphile.
-- 
-- Default user `mediatum` does not have the privilege `CREATEROLE`.
-- So, the role for this user needs to be created by e.g. a superuser.
-- 
-- create role mediatum_view_api login;


-- Grant all privileges needed when accessing the API.

grant usage on schema
    mediatum,
    config,
    preprocess,
    entity,
    aux,
    api
    to mediatum_view_api;

grant select on table
    mediatum.node,
    mediatum.nodetype,
    mediatum.noderelation,
    mediatum.nodemapping,
    mediatum.node_to_access_rule,
    mediatum.access_rule
    to mediatum_view_api;

grant select on table
    preprocess.ufts,
    preprocess.aspect
    to mediatum_view_api;

grant select on all tables in schema
    config,
    entity,
    aux
    to mediatum_view_api;

grant execute on all functions in schema
    api,
    aux
    to mediatum_view_api;
