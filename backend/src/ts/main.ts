
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
const statementTimeout = "30s"

const databaseConnectionUser = TIER == 'dev' ? databaseSuperUser : process.env.MEDIATUM_DATABASE_USER_VIEW_API;
const databaseConnectionUrl = "postgres://" + databaseConnectionUser + "@localhost:5432/" + process.env.MEDIATUM_DATABASE_NAME;

// ----------------------------------------------------------------

const app = express();

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

