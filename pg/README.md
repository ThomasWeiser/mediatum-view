
# mediaTUM View - Backend

## Overview

We add several PostgreSQL views and functions to the original mediaTUM database design.

The new code lives in these PostgreSQL schemas:

| Schema    | Description                              |
| --------- | ---------------------------------------- |
| `entity`  | Views (some of them materialized) that map the original generic node structure to the specific entities of interest. |
| `api`     | Types and functions that define the GraphQL schema and the resolvers implementing the GraphQL operations. |
| `aux`     | Helper functions                         |
| `debug`   | Additional GraphQL functions briding between the original node table and the exported GraphQL schema. |
| `examine` | For experimental SQL code used to examine the original database structure. |

On top of that we use [PostGraphile](https://www.graphile.org/postgraphile/) (formerly known as PostGraphQL), to expose the functions from PostgreSQL schema `api` (and optionaly from schema `debug`) as a GraphQL service.

## Installation

### PostgreSQL Code

To submit the new code to a running mediaTUM database execute the SQL / plpgsql code of the source files listed in `modd.conf`.

Best way to do this is to use the developer tool [modd](https://github.com/cortesi/modd), which also takes care of re-submitting changed files. This tool is available for all major platforms and very easy to install.

Please review the configuration in `modd.conf`. You probably have to tailor the database name and the user name. Then start the tool simply with:

```sh
modd 
```

### PostGraphile

[PostGraphile](https://www.graphile.org/postgraphile/) is a `Node.js` application. Install with:

```sh
npm install -g postgraphile
```

See `bin/start-postgraphile` for the necessary parameters when starting PostGraphile. Please review and customize the configuration in that script, then run:

```sh
bin/start-postgraphile
```

If all goes well you will see two URLs: One for the GraphQL endpoint, and one for [Graph*i*QL](https://github.com/graphql/graphiql). Open the latter to get a handy in-browser tool to explore the resulting API.
