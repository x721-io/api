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
  GetContractQuery,
  GetContractQueryVariables,
} from '../../generated/graphql';
import {
  ErcContractExternalQuery,
  ErcContractExternalQueryVariables,
  getSdk as getSdkExternal,
} from '../../generated/SubgraphExternal/graphql';
import { GraphQLClient } from 'graphql-request';
import SecureUtil from '../../commons/Secure.common';
import axios from 'axios';

interface responseRedisExternal {
  address: string;
  totalNft: string;
  totalOwner: string;
}

@Injectable()
export class GetCollectionMarketData {
  private readonly endpoint = process.env.SUBGRAPH_URL;
  private graphqlClient: GraphQLClient;
  private readonly DeadAddress = '0x000000000000000000000000000000000000dead';

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

  async getContractInfor(collectionAddress: string): Promise<GetContractQuery> {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const variables: GetContractQueryVariables = { address: collectionAddress };
    return await sdk.getContract(variables);
  }

  async getContractInforV2(collectionAddress: string) {
    // console.log(
    //   `${process.env.EXPLORE_API}tokens/${collectionAddress}/counters`,
    // );
    console.log(
      `https://u2uscan.xyz/api/v2/tokens/${collectionAddress}/counters`,
    );
    try {
      const { data } = await axios.get<any, any>(
        `https://u2uscan.xyz/api/v2/tokens/${collectionAddress}/counters`,
      );

      return data;
    } catch (err) {
      return null;
    }
  }

  async getAllCollectionExternal(contract: string) {
    try {
      const response = await SecureUtil.getSessionInfo(`External-${contract}`);
      const result: responseRedisExternal = JSON.parse(response);
      return {
        totalNftExternal: result?.totalNft ? parseInt(result.totalNft) : 0,
        totalOwnerExternal: result?.totalOwner
          ? parseInt(result?.totalOwner)
          : 0,
      };
    } catch (error) {
      console.log('getAllCollectionExternal', error);
    }
  }
}
