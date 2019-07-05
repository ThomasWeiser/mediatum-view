
-- Publicly exposed GraphQL functions
-- regarding folders (i.e. collections and directories).


create or replace function api.all_folders (name text, parent_ids int4[], is_root boolean, is_collection boolean, find text)
    returns setof api.folder as $$
    select * from entity.folder
    where (all_folders.name is null or folder.name = all_folders.name)
      and (all_folders.parent_ids is null or folder.parent_id = any (all_folders.parent_ids))
      and (all_folders.is_root is null or folder.parent_id is null = all_folders.is_root)
      and (all_folders.is_collection is null or folder.is_collection = all_folders.is_collection)
      and (all_folders.find is null or folder.name ilike ('%' || all_folders.find || '%'))
    order by folder.orderpos
$$ language sql stable rows 1000;

comment on function api.all_folders (name text, parent_ids int4[], is_root boolean, is_collection boolean, find text) is
    'Reads and enables pagination through all folders (i.w. collections and directories), optionally filtered by name, parentId, isRoot and isCollection, and searchable by name.';


create or replace function api.folder_by_id (id int4)
    returns api.folder as $$
    select * from entity.folder
    where entity.folder.id = folder_by_id.id
$$ language sql stable;

comment on function api.folder_by_id (id int4) is
    'Gets a folder by its mediaTUM node id.';


create or replace function api.folder_subfolders (parent api.folder, name text, is_collection boolean, find text)
    returns setof api.folder as $$
    select * from entity.folder
    where folder.parent_id = parent.id
      and (folder_subfolders.name is null or folder.name = folder_subfolders.name)
      and (folder_subfolders.is_collection is null or folder.is_collection = folder_subfolders.is_collection)
      and (folder_subfolders.find is null or folder.name ilike ('%' || folder_subfolders.find || '%'))
    order by folder.orderpos
$$ language sql stable rows 10;

comment on function api.folder_subfolders (parent api.folder, name text, is_collection boolean, find text) is
    'Reads and enables pagination through all sub-folders of this folder, optionally filtered by name and isCollection, and searchable by name.';


create or replace function api.folder_superfolder (child api.folder)
    returns api.folder as $$
    select * from entity.folder
    where folder.id = child.parent_id
$$ language sql stable;

comment on function api.folder_superfolder (child api.folder) is
    'Gets the super-folder of this folder. Returns null if this folder is at the root.';


create or replace function api.folder_lineage (current_folder api.folder)
    returns api.folder[] as $$
    declare lineage api.folder[] := array[current_folder];
    begin
        loop
            current_folder := api.folder_superfolder (current_folder);
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
