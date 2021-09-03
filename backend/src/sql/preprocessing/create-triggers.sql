
drop trigger if exists trigger_ufts_on_node_upsert on mediatum.node;

create trigger trigger_ufts_on_node_upsert
after insert or update
    on mediatum.node
    for each row
    execute function preprocess.update_ufts_on_node_upsert()
;


drop trigger if exists trigger_aspect_on_node_upsert on mediatum.node;

create trigger trigger_aspect_on_node_upsert
after insert or update
    on mediatum.node
    for each row
    execute function preprocess.update_aspect_on_node_upsert()
;
