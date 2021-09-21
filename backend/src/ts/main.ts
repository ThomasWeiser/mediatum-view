

const express = require("express");
const { postgraphile } = require("postgraphile");

const app = express();


const databaseSuperUser = "postgres"; // Necessary for using "watch" option
const port = 5000;
const statementTimeout = "30s"

const databaseConnectionUrl = "postgres://" + databaseSuperUser + "@localhost:5432/" + process.env.MEDIATUM_DATABASE_NAME;

app.use(
    postgraphile(
        databaseConnectionUrl,
        "api",
        {
            pgDefaultRole: process.env.MEDIATUM_DATABASE_USER_VIEW_API,
            enableCors: true,
            exportGqlSchemaPath: 'export/schema-export.graphql',
            watchPg: true,
            graphiql: true,
            enhanceGraphiql: true,
            setofFunctionsContainNulls: false,
            legacyRelations: "omit",
            ignoreRBAC: false,
            pgSettings: {
                statement_timeout: statementTimeout,
            },
            allowExplain: false
        }
    )
);

app.listen(port);

console.log(`mediaTUM View - PostGraphile listening on port ${port}`);

