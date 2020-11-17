

drop table if exists preprocess.mfts;  

create table preprocess.mfts (
	nid serial references mediatum.node(id) on delete cascade,
    key text,
    value text,
    value_normalized text,
	tsvec tsvector null,
    primary key (nid, key)
);

drop view if exists preprocess.mfts_as_view;

create view preprocess.mfts_as_view as
    select
        node.id as nid,
        attr.key as key,
        attr.value as value,
        aux.normalize_facet_value(attr.value) as value_normalized,
        to_tsvector('english_german', left(attr.value, 1048000)) as tsvec
    from
        mediatum.node,
        jsonb_each_text(node.attrs) as attr
    where 
        -- TODO: Is this the best way to identify documents?
        -- TODO: Apply also in ufts preprocessing
        -- TODO: Check handling of emtpy and null values
        node.schema is not null
        and not aux.nodetype_is_container (node.type)
;

delete from preprocess.mfts;   

-- Suppress notices about "Word is too long to be indexed. Words longer than 2047 characters are ignored."
-- We don't mind that such long lexemes don't get indexed.
set session client_min_messages to warning;

insert into preprocess.mfts (nid, key, value, value_normalized, tsvec)
  select * 
  from preprocess.mfts_as_view
  -- limit 200000 -- For testing the code one may just process a small fraction of the data
;

-- Reset message level to default
set session client_min_messages to notice;

create index if not exists mfts_rum_tsvector_ops
    on preprocess.mfts
 using rum (tsvec rum_tsvector_ops)
;

create index if not exists mfts_rum_tsvector_addon_ops
    on preprocess.mfts
 using rum (tsvec rum_tsvector_addon_ops, key)
  with (attach ='key', to = 'tsvec')
 ;

