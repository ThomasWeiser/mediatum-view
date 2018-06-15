
# mediaTUM View - Frontend

## Installation

You need to have [Elm](http://elm-lang.org/) v0.18 installed. See [here](https://guide.elm-lang.org/install.html) for platform-specific instructions.

Get the needed Elm packages from the official repository:

```sh
elm package install
```

We use [GraphqElm](https://github.com/dillonkearns/graphqelm) to autogenerate GraphQL queries in Elm. Install and run the tool:

```sh
npm install -g graphqelm

graphqelm http://localhost:5000/graphql --base Graphql --output src
```

To build the app we currently use the tool [elm-live](https://github.com/tomekwi/elm-live). It watches the Elm source code, compiles it when needed, serves the result via HTTP and handles live reloading. Install with:

```sh
npm install -g elm-live
```

Then start elm-live via this script:

```sh
bin/start
```

and open http://localhost:8000/ to run the app in your browser.
