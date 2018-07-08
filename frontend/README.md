
# mediaTUM View - Frontend

## Prerequisites

You need to have [Elm](http://elm-lang.org/) v0.18 installed. See [here](https://guide.elm-lang.org/install.html) for platform-specific instructions.

We use [GraphqElm](https://github.com/dillonkearns/graphqelm) to autogenerate GraphQL queries in Elm. Install this tool via npm with:

```sh
npm install -g graphqelm
```

Additionally, we use the tool [elm-live](https://github.com/tomekwi/elm-live). It watches the Elm source code, recompiles it when needed, serves the result via HTTP and handles live reloading the browser. Install with:

```sh
npm install -g elm-live
```

## Building

The commands to build the frontend app are in the script `bin/build`. Please review this file. Before running it make sure that the backend is already started. Then run the script:

```sh
bin/build
```

For serving the the app and live rebuilding start `elm-live` with this script:

```sh
bin/start
```

and open http://localhost:8000/ to run the app in your browser.

## Debugging

As an alternative to the official Elm debugger we utilize the tool [elm-remotedev](https://github.com/utkarshkukreti/elm-remotedev):

> Integration of Elm Application's `update` function with Redux DevTools 
> Extension.

The [Redux DevTools Extension](https://github.com/zalmoxisus/redux-devtools-extension) are available [for Firefox](https://addons.mozilla.org/en-US/firefox/addon/remotedev/) and [for Chromium/Chrome](https://chrome.google.com/webstore/detail/redux-devtools/lmhkpmbekcpmknklioeibfkpmmfibljd).
