
# mediaTUM View - Backend

## Overview

We add several PostgreSQL views and functions to the original mediaTUM database design.

The new code lives in these PostgreSQL schemas:

| Schema    | Description                              |
| --------- | ---------------------------------------- |
| `config`  | Tables for configuring the application (top-level folders, aspects, masks) |
| `preprocess`  | Tables `ufts` and `aspect` with preprocessed `tsvector` values for full text search. |
| `entity`  | Views (some of them materialized) that map the original generic node structure to the specific entities of interest. |
| `api`     | Types and functions that define the GraphQL schema and the resolvers implementing the GraphQL operations. |
| `aux`     | Helper functions                         |

On top of that we use [PostGraphile](https://www.graphile.org/postgraphile/) (formerly known as PostGraphQL), to expose the functions from PostgreSQL schema `api` (and optionally from schema `debug`) as a GraphQL service.

## Prerequisites

### PostgreSQL

In general, we use an up-to-date version of PostgreSQL. 

Currently we are using version 13.3.

Minimum required version is 10.

### Configuration Variables

We use environment variables to configure the database name and the relevant database users.

```sh
$ export MEDIATUM_DATABASE_NAME="mediatum"
$ export MEDIATUM_DATABASE_USER="mediatum"
$ export MEDIATUM_DATABASE_USER_VIEW_API="mediatum_view_api"
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

[PostGraphile](https://www.graphile.org/postgraphile/) is a `Node.js` application, installed utilizing _npm_:

```sh
$ npm install
```

## Installation

The new code lives in dedicated schemas as noted above. 
In order to create those schemas you may have to grant the permission to do so to the database user that is used to build the database application:

```sh
$ psql -d $MEDIATUM_DATABASE_NAME -c "GRANT CREATE ON DATABASE $MEDIATUM_DATABASE_NAME TO $MEDIATUM_DATABASE_USER;"
```

We need a new database user with restricted privileges that will be used to access the API functions.
Note that to create this user you need the privilege `CREATEROLE`, e.g. as a superuser.

```sh
$ psql -d $MEDIATUM_DATABASE_NAME -c "CREATE ROLE $MEDIATUM_DATABASE_USER_VIEW_API LOGIN;"
```

To submit the new code to a running mediaTUM database execute the SQL and PL/pgSQL code with the appropriate scripts in `bin/`.
Please review these scripts before using them.

```sh
$ bin/build-init
$ bin/build-preprocess
$ bin/build-api
$ bin/preprocess-all
$ bin/enable-update-triggers
$ bin/preprocess-catch-up
```

Preprocessing the fulltext and meta-data to build the FTS index takes some time, e.g. about 60 minutes for a mediaTUM installation with 600000 nodes.

You may drop all changes introduced by this backend via `bin/drop-all`.

You may also want to drop the newly introduced role used for accessing the API:

```sh
$ psql -d $MEDIATUM_DATABASE_NAME -c "DROP OWNED BY $MEDIATUM_DATABASE_USER_VIEW_API; DROP ROLE $MEDIATUM_DATABASE_USER_VIEW_API;"
```

## Running PostGraphile

See `bin/start` for the necessary parameters when starting PostGraphile. Please review and customize the configuration in that script (e.g. database connection and binding the service to a network interface), then run:

```sh
$ bin/start
```

If all goes well you will see two URLs: One for the GraphQL endpoint, and one for [Graph*i*QL](https://github.com/graphql/graphiql). You may want to open the latter for a handy in-browser tool to explore the resulting API and its built-in documentation.

## Database Maintenance

You probably want to enable updating of the preprocessed data automatically by running the script `bin/enable-update-triggers`.
Alternatively you may use the script `bin/preprocess-catch-up` periodically to add missing preprocessed data.

Note that changes to the table `config.aspect_def` don't trigger an update of table `preprocess.aspect` automatically. Please run `bin/preprocess-catch-up` manually in this case. This will also delete obsolete preprocessed aspect data if you have removed some aspect definitions.

We use some materialized views for performance reasons.
Currently these are: `folder`, `folder_node`, `mapping`, `mappingfield`, `mask`, `maskitem`, `metadatatype`, `metafield`, which are all defined within schema `entity`. They are all derived from the related specialized node types from table `mediatum.node`.

These materialized views need to be kept up-to-date via the script `bin/refresh-materialized-views`, either periodically or when needed.
Refreshing needs about 40 seconds, so it may be run e.g. hourly.
