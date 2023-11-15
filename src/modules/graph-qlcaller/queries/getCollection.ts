// src/graphql/queries/getUser.query.ts

export const GET_COLLECTION_QUERY = `
query MyQuery($first: Int!) {
    collections(first: $first) {
      id
      owner
      txCreation
      name
      symbol
    }
  }   
`;
