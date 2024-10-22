
import type { CodegenConfig } from '@graphql-codegen/cli';

import { config as dotenvConfig } from 'dotenv';
dotenvConfig();

const config: CodegenConfig = {
  overwrite: true,
  // schema: process.env.SUBGRAPH_URL,
  // documents: 'src/graph-query/*.graphql',
  generates: {
    "src/generated/graphql.ts": {
      schema: process.env.SUBGRAPH_URL,
      documents: 'src/graph-query/*.graphql',
      plugins: ["typescript", "typescript-resolvers", "typescript-graphql-request", "typescript-operations"]
    },
    "src/generated/Template1155/graphql.ts": {
      schema: process.env.SUBGRAPH_TEMPLATE_1155 as string,
      documents: 'src/graph-query-template-1155/*.graphql',
      plugins: ["typescript", "typescript-resolvers", "typescript-graphql-request", "typescript-operations"]
    },
    "src/generated/Template721/graphql.ts": {
      schema: process.env.SUBGRAPH_TEMPLATE_721 as string,
      documents: 'src/graph-query-template-721/*.graphql',
      plugins: ["typescript", "typescript-resolvers", "typescript-graphql-request", "typescript-operations"]
    },
    "src/generated/SubgraphExternal/graphql.ts": {
      schema: process.env.SUBGRAPH_EXTERNAL_URL as string,
      documents: 'src/graph-query-external/*.graphql',
      plugins: ["typescript", "typescript-resolvers", "typescript-graphql-request", "typescript-operations"]
    },
  }
};

export default config;
