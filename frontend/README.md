
# mediaTUM View - Frontend

## Prerequisites

The prerequisites can all be installed as npm packages into the project directory.
There is a `package.json` for this purpose.

```sh
$ npm install
```

Alternatively, you may choose to install the tools individually.

- [Elm](http://elm-lang.org/) v0.19. See [here](https://guide.elm-lang.org/install.html) for platform-specific instructions.
- [elm-graphql](https://github.com/dillonkearns/elm-graphql) to autogenerate GraphQL queries in Elm.
- [elm-live](https://github.com/tomekwi/elm-live), which watches the Elm source code, recompiles it when needed, serves the result via HTTP and handles live reloading the browser.

## Building

The commands to build the frontend app are available as npm scripts. They use the tools installed by `npm install`.

Please review the scripts in `package.json`.
They need to specify where the backend service is running, e.g. `localhost:5000`.

Before starting the build make sure that the backend is already started. Then call:

```sh
$ npm run build
```

For serving the app, live rebuilding and proxying the backend start `elm-live` with this script:

```sh
$ npm run start
```

Then open the app at http://localhost:8000/ in your browser.
