
-- Drop all schemas introduced specifically by this backend.


begin;

drop schema if exists aux, entity, api, debug, examine CASCADE;

commit;
