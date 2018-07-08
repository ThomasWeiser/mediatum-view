
# mediaTUM View - Backend

## Overview

We add several PostgreSQL views and functions to the original mediaTUM database design.

The new code lives in these PostgreSQL schemas:

| Schema    | Description                              |
| --------- | ---------------------------------------- |
| `entity`  | Views (some of them materialized) that map the original generic node structure to the specific entities of interest. |
| `api`     | Types and functions that define the GraphQL schema and the resolvers implementing the GraphQL operations. |
| `aux`     | Helper functions                         |
| `debug`   | Additional GraphQL functions conveying between the original node table and the exported GraphQL schema. |
| `examine` | For experimental SQL code used to examine the original database structure. |

On top of that we use [PostGraphile](https://www.graphile.org/postgraphile/) (formerly known as PostGraphQL), to expose the functions from PostgreSQL schema `api` (and optionaly from schema `debug`) as a GraphQL service.

## Prerequisites

[PostGraphile](https://www.graphile.org/postgraphile/) is a `Node.js` application. Install with:

```sh
npm install -g postgraphile
```

## Installation

To submit the new code to a running mediaTUM database execute the SQL and PL/pgSQL code as listed in `bin/build`. Please review this script, especially the the name of the database, and then run it with

```sh
bin/build
```

## Running PostGraphile

See `bin/start` for the necessary parameters when starting PostGraphile. Please review and customize the configuration in that script, then run:

```sh
bin/start
```

If all goes well you will see two URLs: One for the GraphQL endpoint, and one for [Graph*i*QL](https://github.com/graphql/graphiql). Open the latter to get a handy in-browser tool to explore the resulting API.
