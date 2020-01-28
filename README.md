
# mediaTUM View

## Background

[__mediaTUM__](https://mediatum.github.io/en.index.html) is a media and publications repository. It supports the publication of digital documents and research data as well as the use of multimedia content in research and teaching.

The current main implementation is based on PostgreSQL and Python, available at [GitHub](https://github.com/mediatum/mediatum).

mediaTUM is in use as the central document and publication server of the [Technical University of Munich](https://mediatum.ub.tum.de/). More instances are in use at [University of Augsburg](https://media.bibliothek.uni-augsburg.de/), [Bundeswehr University Munich](https://athene-forschung.unibw.de/) and [Catholic University of Eichstätt-Ingolstadt](https://media.ku.de/).

## An alternative API and client for mediaTUM

__mediaTUM View__ is a reimplementation of the public web interface of mediaTUM. It provides a new API and a single-page web application for searching and document access.

### Architecture Overview

1. Utilization of the existing database structure used by the original Python implementation of mediaTUM.
2. Additional PostgreSQL schemas, to provide the relevant data transformed into a user-centric representation.
3. [PostGraphile](https://www.graphile.org/postgraphile/) to expose a public schema as a GraphQL API.
4. A web-app client written in [Elm](https://elm-lang.org/) to search and browse (part of) the mediaTUM content.

## Installation

See instructions for [backend](backend/README.md) and [frontend](frontend/README.md).
