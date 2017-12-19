
# mediaTUM View
## An alternative API and client for mediaTUM

_... work in progress ..._

### Architecture Overview

1. Utilization of the existing database structure used by the original Python imlementation of [mediaTUM](https://github.com/mediatum/mediatum).
2. Additional PostgreSQL schemas, to provide the relevant information transformed into a user-centric representation
3. [PostGraphile](https://www.graphile.org/postgraphile/) to expose a public schema as a GraphQL API
4. A web-app client written in [Elm](http://elm-lang.org/) to search and browse (part of) the mediaTUM content

