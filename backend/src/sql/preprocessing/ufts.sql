
-- Preprocess data for FTS and create appropriate indexes

create or replace function preprocess.year_from_attrs
    ( attrs jsonb
    )
    returns int4 as $$
    begin
        return
            -- Try to extract a 4 digits year substring from different possible date formats used in attribute field "year"
            substring((attrs #>> '{year}') from '\d{4}')::int4;
        
        exception when others then
                raise notice 'invalid year: "%".  returning null.', (attrs #>> '{year}');
                return null;
    end;
$$ language plpgsql immutable;

create or replace function preprocess.unified_tsvec_from_attrs_and_fulltext
    ( attrs jsonb
    , fulltext varchar
    )
    returns tsvector as $$
    declare
        exc_text text;
        exc_detail text;
        exc_hint text;
    begin
	    return
			setweight(
                to_tsvector(
                    'english_german',
                    coalesce (attrs, '{}'::jsonb)
                    ),
                'A')
			||
			setweight(
                to_tsvector(
                    'english_german'
                    -- Limit the fulltext length to avoid "string is too long for tsvector (... bytes, max 1048575 bytes)"
                    , left(
                        coalesce (fulltext, ''),
                        1048000)
                    ),
                'D');

        exception
            when others then
                get stacked diagnostics exc_text = message_text,
                                        exc_detail = pg_exception_detail,
                                        exc_hint = pg_exception_hint;
                raise notice 'Exception:  %\n%\nHint: %', exc_text, exc_detail, exc_hint;

                return null;
    end;
$$ language plpgsql immutable;


create or replace view preprocess.ufts_as_view as
    select
    node.id as nid,
    preprocess.year_from_attrs(node.attrs) as year,
    - node.id as recency,
    preprocess.unified_tsvec_from_attrs_and_fulltext(node.attrs, node.fulltext) as tsvec
    from mediatum.node
;


create or replace function preprocess.update_ufts_on_node_upsert()
    returns trigger
    as $$
    begin
        insert into preprocess.ufts (nid, year, recency, tsvec)
            select nid, year, recency, tsvec
            from preprocess.ufts_as_view
            where ufts_as_view.nid = new.id
            and (nid >= current_setting('mediatum.preprocessing_min_node_id', true)::int) is not false
            and (nid <= current_setting('mediatum.preprocessing_max_node_id', true)::int) is not false
            on conflict on constraint ufts_pkey
            do update set 
                year = excluded.year,
                recency = excluded.recency,
                tsvec = excluded.tsvec
        ;

        return new;
    end;
$$ language plpgsql;
