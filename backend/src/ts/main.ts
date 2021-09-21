
const express = require("express");
const { postgraphile } = require("postgraphile");

type Tier = 'dev' | 'prod';
const validTiers = ['dev', 'prod'];

const TIER = process.env.TIER as Tier;

if (!TIER || !validTiers.includes(TIER)) {
    console.error(`The TIER environment variable is required but was either not specified or is set to an invalid value: ${String(TIER)}.`);
    process.exit(1);
}

const databaseSuperUser = "postgres"; // Necessary for using "watch" option in dev
const port = 5000;
const statementTimeout = "30s";
const rateLimitWindow = 1 * 60 * 1000; // 60 seconds
const rateLimitCount = 100; // limit each key (i.e. IP) to 100 requests per window

const databaseConnectionUser = TIER == 'dev' ? databaseSuperUser : process.env.MEDIATUM_DATABASE_USER_VIEW_API;
const databaseConnectionUrl = "postgres://" + databaseConnectionUser + "@localhost:5432/" + process.env.MEDIATUM_DATABASE_NAME;

// ----------------------------------------------------------------

const app = express();
app.disable('x-powered-by');


import expressCompression = require('compression');
app.use(expressCompression({ threshold: 0 }));


/* You may use helmet ("Express.js security with HTTP headers") here:
import helmet = require('helmet');
app.use(helmet());
*/


/* You may use morgan ("HTTP request logger middleware") here:
// Should be configured to your needs.
import morgan = require('morgan');
app.use(morgan('tiny'));
*/


import rateLimit from "express-rate-limit";
// Enable if you're behind a reverse proxy (Heroku, Bluemix, AWS ELB, Nginx, etc)
// see https://expressjs.com/en/guide/behind-proxies.html
// app.set('trust proxy', 1);

app.use(rateLimit({
    windowMs: rateLimitWindow,
    max: rateLimitCount,
    headers: false
}));


app.use(
    postgraphile(
        databaseConnectionUrl,
        "api",
        {
            pgDefaultRole: process.env.MEDIATUM_DATABASE_USER_VIEW_API,
            enableCors: true,
            exportGqlSchemaPath: TIER == 'dev' ? 'export/schema-export.graphql' : null,
            watchPg: TIER == 'dev',
            graphiql: TIER == 'dev',
            enhanceGraphiql: TIER == 'dev',
            allowExplain: TIER == 'dev',
            setofFunctionsContainNulls: false,
            legacyRelations: "omit",
            ignoreRBAC: false,
            pgSettings: {
                statement_timeout: statementTimeout,
            }
        }
    )
);

app.listen(port);

console.log(`mediaTUM View - PostGraphile listening on port ${port}`);

