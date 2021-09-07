
do
  $$
    declare
      required_extensions text[] = array
        [ 'rum'
        , 'snowball_bilingual'
        ];

      missing_extensions text[] =
        array(
          select unnest(required_extensions)
          except
          select extname from pg_extension
        );

    begin
      if cardinality(missing_extensions) > 0 then
        raise exception 
          'missing required extension(s): %',
          array_to_string(missing_extensions, ', ');
      end if;
    end;
  $$
;
