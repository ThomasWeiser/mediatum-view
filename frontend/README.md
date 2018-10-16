
# mediaTUM View - Frontend

## Prerequisites

The prerequisites can all be installed as npm packages.
There is a `package.json` solely for this purpose.
Alternatively, you may also choose to install the tools individually.

- [Elm](http://elm-lang.org/) v0.19. See [here](https://guide.elm-lang.org/install.html) for platform-specific instructions.
- [elm-graphql](https://github.com/dillonkearns/elm-graphql) to autogenerate GraphQL queries in Elm.
- [elm-live](https://github.com/tomekwi/elm-live), which watches the Elm source code, recompiles it when needed, serves the result via HTTP and handles live reloading the browser.

## Building

The commands to build the frontend app are in the script `bin/build`. Please review this file. Before running it make sure that the backend is already started. Then run the script:

```sh
$ bin/build
```

For serving the the app and live rebuilding start `elm-live` with this script:

```sh
$ bin/start
```

and open http://localhost:8000/ to run the app in your browser.

