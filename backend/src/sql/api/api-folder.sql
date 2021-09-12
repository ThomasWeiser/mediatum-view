
-- Publicly exposed GraphQL functions
-- regarding folders (i.e. collections and directories).


create or replace function api.all_folders
    ( ids int4[]
    , parent_ids int4[]
    , is_root boolean
    )
    returns setof api.folder as $$
    select * from entity.folder
    where (all_folders.ids is null or folder.id = any (all_folders.ids))
      and (all_folders.parent_ids is null or folder.parent_id = any (all_folders.parent_ids))
      and (all_folders.is_root is null or folder.parent_id is null = all_folders.is_root)
    order by folder.orderpos
$$ language sql stable rows 1000;

comment on function api.all_folders (ids int4[], parent_ids int4[], is_root boolean) is
    'Reads and enables pagination through all folders (i.w. collections and directories),'
    ' optionally filtered a list of ids, a list of parentIds and isRoot.';


create or replace function api.folder_subfolders
    ( parent api.folder
    )
    returns setof api.folder as $$
    select * from entity.folder
    where folder.parent_id = parent.id
    order by folder.orderpos
$$ language sql stable rows 10;

comment on function api.folder_subfolders (parent api.folder) is
    'Reads and enables pagination through all sub-folders of this folder.';


create or replace function api.folder_lineage
    ( current_folder api.folder
    )
    returns api.folder[] as $$
    declare lineage api.folder[] := array[current_folder];
    begin
        loop
            current_folder := entity.superfolder (current_folder);
            if current_folder is null then
                exit;
            else
                lineage := lineage || current_folder;
            end if;
        end loop;
        return lineage;
    end;
$$ language plpgsql stable;

comment on function api.folder_lineage (current_folder api.folder) is
    'Gets a list of folders representing the path from the folder up to the root of the hierarchy.';


