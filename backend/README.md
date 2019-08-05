
# mediaTUM View - Backend

## Overview

We add several PostgreSQL views and functions to the original mediaTUM database design.

The new code lives in these PostgreSQL schemas:

| Schema    | Description                              |
| --------- | ---------------------------------------- |
| `preprocess`  | A table with preprocessed `tsvector` values for full text search. |
| `entity`  | Views (some of them materialized) that map the original generic node structure to the specific entities of interest. |
| `api`     | Types and functions that define the GraphQL schema and the resolvers implementing the GraphQL operations. |
| `aux`     | Helper functions                         |
| `debug`   | Additional GraphQL functions conveying between the original node table and the exported GraphQL schema. |
| `examine` | For experimental SQL code used to examine the original database structure. |

On top of that we use [PostGraphile](https://www.graphile.org/postgraphile/) (formerly known as PostGraphQL), to expose the functions from PostgreSQL schema `api` (and optionally from schema `debug`) as a GraphQL service.

## Prerequisites

### PostgreSQL

In general, we use an up-to-date version of PostgreSQL. 

Currently we are using version 11.4.

Minimum required version is 10.

### Configuration Variables

We use environment variables to configure the database name and the database user.

```sh
$ export MEDIATUM_DATABASE_NAME="mediatum"
$ export MEDIATUM_DATABASE_USER="mediatum"
```

### RUM

[RUM](https://github.com/postgrespro/rum) is an extension for PostgreSQL that adds a new indexing method, similar to GIN.
We use it for efficient ranked full-text search.

To install and integrate RUM do something like the following.

```sh
$ git clone https://github.com/postgrespro/rum
$ cd rum
$ make USE_PGXS=1

$ sudo make USE_PGXS=1 install

$ make USE_PGXS=1 installcheck
$ dropdb contrib_regression

$ psql -d $MEDIATUM_DATABASE_NAME -c "CREATE EXTENSION rum;"
```

### Bilingual Dictionary

In order to make full text search working with English and German texts
we use a custom made dictionary for configuring FTS in PostgreSQL.
This dictionary (`english_german`) is similar to the
[builtin Snowball stemmers](https://www.postgresql.org/docs/current/textsearch-dictionaries.html#TEXTSEARCH-SNOWBALL-DICTIONARY),
but uses both language settings for stemming and produces lexeme variants if the stems differ.

The dictionary is realized as a PostgreSQL extension `snowball_bilingual`.
It's source code is included in this repository.

To build and install it do something like the following.

```sh
$ cd backend/pg-extensions/snowball_bilingual
$ make

$ sudo make install

$ make installcheck
$ dropdb contrib_regression

$ psql -d $MEDIATUM_DATABASE_NAME -c "CREATE EXTENSION snowball_bilingual;"
```

### PostGraphile

[PostGraphile](https://www.graphile.org/postgraphile/) is a `Node.js` application. Install with:

```sh
$ npm install -g postgraphile
```

## Installation

The new code lives in dedicated schemas as noted above. 
In order to create those schemas you may have to grant the permission to do so to the database user:

```sh
$ psql -d $MEDIATUM_DATABASE_NAME -c "GRANT CREATE ON DATABASE $MEDIATUM_DATABASE_NAME TO $MEDIATUM_DATABASE_USER;"
```

To submit the new code to a running mediaTUM database execute the SQL and PL/pgSQL code as listed in `bin/preprocess` and `bin/build`.
Please review these scripts before using them.

Preprocessing the fulltext data to build the FTS index takes some time, e.g. about 30 minutes for a mediaTUM installation with 300000 nodes.

```sh
$ bin/preprocess
$ bin/build
```

## Running PostGraphile

See `bin/start` for the necessary parameters when starting PostGraphile. Please review and customize the configuration in that script (e.g. database connection and binding the service to a network interface), then run:

```sh
$ bin/start
```

If all goes well you will see two URLs: One for the GraphQL endpoint, and one for [Graph*i*QL](https://github.com/graphql/graphiql). You may want to open the latter for a handy in-browser tool to explore the resulting API and its built-in documentation.
