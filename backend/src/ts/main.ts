
import express from 'express';
import { postgraphile, PostGraphileOptions } from "postgraphile";
import { GraphQLError } from "graphql";
import bunyan from 'bunyan';

var log = bunyan.createLogger({ name: 'mediatum-view' });

type Tier = 'dev' | 'prod';
const validTiers = ['dev', 'prod'];

const TIER = process.env.TIER as Tier;

if (!TIER || !validTiers.includes(TIER)) {
    console.error(`The TIER environment variable is required but was either not specified or is set to an invalid value: ${String(TIER)}.`);
    process.exit(1);
}

const databaseSuperUser = 'postgres'; // Necessary when using "watch" option in dev
const port = 5000;
const statementTimeout = '30s';
const rateLimitWindow = 10 * 60 * 1000; // 10 minutes
const rateLimitCount = 500; // limit each key (i.e. IP) to 500 requests per window
const speedLimitWindows = 30 * 60 * 1000;
const speedLimitAfter = 100; // allow 100 unlimited requests per window, then...
const speedLimitDelay = 2; // ... begin adding 2ms of delay per request

const databaseConnectionUser = TIER == 'dev' ? databaseSuperUser : process.env.MEDIATUM_DATABASE_USER_VIEW_API;
const databaseConnectionUrl = `postgres://${databaseConnectionUser}@localhost:5432/${process.env.MEDIATUM_DATABASE_NAME}`;

// ----------------------------------------------------------------

const app = express();
app.disable('x-powered-by');


import expressCompression from 'compression';
app.use(expressCompression({ threshold: 0 }));


/* You may use helmet ("Express.js security with HTTP headers") here:
import helmet from 'helmet';
app.use(helmet());
*/


/* You may use morgan ("HTTP request logger middleware") here:
// Should be configured to your needs.
import morgan from 'morgan';
app.use(morgan('tiny'));
*/


import rateLimit from 'express-rate-limit';
app.use(rateLimit({
    windowMs: rateLimitWindow,
    max: rateLimitCount,
    headers: false
}));


import slowDown from 'express-slow-down';
app.use(slowDown({
    windowMs: speedLimitWindows,
    delayAfter: speedLimitAfter,
    delayMs: speedLimitDelay
}));


const exposedSchema = 'api';

const postgraphileOptionsCommon: PostGraphileOptions = {
    pgDefaultRole: process.env.MEDIATUM_DATABASE_USER_VIEW_API,
    enableCors: true,
    setofFunctionsContainNulls: false,
    legacyRelations: 'omit',
    ignoreRBAC: false,
    pgSettings: {
        statement_timeout: statementTimeout,
    }
};

const postgraphileOptionsDev: PostGraphileOptions = {
    extendedErrors: ["hint", "detail", "errcode"],
    // extendedErrors: ['severity', 'code', 'detail', 'hint', 'position', 'internalPosition', 'internalQuery', 'where', 'schema', 'table', 'column', 'dataType', 'constraint', 'file', 'line', 'routine'],
    // showErrorStack: true,
    exportGqlSchemaPath: 'export/schema-export.graphql',
    watchPg: true,
    graphiql: true,
    enhanceGraphiql: true,
    allowExplain: true,
};

const postgraphileOptionsProd: PostGraphileOptions = {
    handleErrors: handleErrorsProd,
    watchPg: false,
    graphiql: false
};

function handleErrorsProd(errors: readonly GraphQLError[]): any[] {
    return errors.map((error) => {
        log.error({
            graphQLError: {
                message: error.message,
                path: error.path
            }
        });
        return {
            // This is the GraphQL error response to the client.
            // We will not expose any error details in production mode.
            message: 'API error'
        };
    });
}

const postgraphileOptions: PostGraphileOptions = {
    ...postgraphileOptionsCommon
    , ...(TIER == 'dev' ? postgraphileOptionsDev : {})
    , ...(TIER == 'prod' ? postgraphileOptionsProd : {})
}


app.use(
    postgraphile(
        databaseConnectionUrl,
        exposedSchema,
        postgraphileOptions
    )
);

app.listen(port, () => {
    console.log(`mediaTUM View - PostGraphile listening on port ${port} `);
});


