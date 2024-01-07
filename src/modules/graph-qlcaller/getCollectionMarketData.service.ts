import { Injectable } from '@nestjs/common';
import {
  getSdk,
  GetCollectionsDataQueryVariables,
  GetCollectionsDataQuery,
  GetCollectionHoldersQuery,
  GetCollectionTokensQueryVariables,
  GetCollectionTokensQuery,
  ErcContractQuery,
  ErcContractQueryVariables,
} from '../../generated/graphql';
import { GraphQLClient } from 'graphql-request';
@Injectable()
export class GetCollectionMarketData {
  private readonly endpoint = process.env.SUBGRAPH_URL;
  private graphqlClient: GraphQLClient;

  constructor() {
    this.graphqlClient = new GraphQLClient(this.endpoint);
  }

  private getGraphqlClient() {
    return new GraphQLClient(this.endpoint);
  }

  async getCollectionSumData(
    collectionAddress: string,
  ): Promise<GetCollectionsDataQuery> {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const variables: GetCollectionsDataQueryVariables = { collectionAddress };
    const reponse = await sdk.GetCollectionsData(variables);
    return reponse;
  }

  async getCollectionHolder(
    collectionAddress: string,
  ): Promise<GetCollectionHoldersQuery> {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const variables: GetCollectionsDataQueryVariables = { collectionAddress };
    return await sdk.GetCollectionHolders(variables);
  }

  async getCollectionTokens(
    collectionAddress: string,
  ): Promise<GetCollectionTokensQuery> {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const variables: GetCollectionTokensQueryVariables = { collectionAddress };
    return await sdk.GetCollectionTokens(variables);
  }

  async getCollectionCount(
    collectionAddress: string,
  ): Promise<ErcContractQuery> {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const variables: ErcContractQueryVariables = { id: collectionAddress };
    return await sdk.ErcContract(variables);
  }
}
