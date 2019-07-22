[![GitHub license](https://img.shields.io/badge/license-PostgreSQL-blue.svg)](LICENSE)

# snowball_bilingual

The `snowball_bilingual` extension provides a new dictionary template. It is a copy of
the [Snowball dictionary template](https://www.postgresql.org/docs/current/static/textsearch-dictionaries.html#TEXTSEARCH-SNOWBALL-DICTIONARY)
of PostgreSQL. Currently it provides stemming algorithms for the following
languages:
 * nepali

The reason why `snowball_bilingual` exists is that PostgreSQL's Snowball dictionary
template isn't extensible. That is you cannot add new stemming algorithms.

## Installation

To install `snowball_bilingual`, execute this in the extension's directory:

```shell
make install USE_PGXS=1
```

> **Important:** Don't forget to set the `PG_CONFIG` variable
> (`make PG_CONFIG=...`) in case you want to test `snowball_bilingual` on a
> non-default or custom build of PostgreSQL. Read more
> [here](https://wiki.postgresql.org/wiki/Building_and_Installing_PostgreSQL_Extension_Modules).

After that, execute the following query:
```plpgsql
CREATE EXTENSION snowball_bilingual;
```

It's done. Now you can test a new text search dictionary:

```plpgsql
SELECT to_tsvector('nepali', 'अँगअँकाउछन्');
 to_tsvector
-------------
 'अँगअँकाउ':1
```

## Authors

 * Arthur Zakirov <a.zakirov@postgrespro.ru>, Postgres Professional Ltd.
 * Ingroj Shrestha <ing.stha@gmail.com>, Nepali NLP Group
 * Oleg Bartunov <obartunov@gmail.com>, Postgres Professional Ltd.
 * Shreeya Singh Dhakal, Nepali NLP Group