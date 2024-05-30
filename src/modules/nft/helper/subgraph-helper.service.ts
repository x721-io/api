import { ApiCallerService } from 'src/modules/api-caller/api-caller.service';
import { CONTRACT_TYPE } from '@prisma/client';
import { gql, GraphQLClient } from 'graphql-request';
import {
  getSdk as getSdk1155,
  GetBalances1155QueryVariables,
} from 'src/generated/Template1155/graphql';
import {
  getSdk as getSdk721,
  GetBalances721QueryVariables,
} from 'src/generated/Template721/graphql';

class subgraphServiceCommon {
  apiService: ApiCallerService;

  async subgraphQuery(url: string, type: CONTRACT_TYPE, tokenId: string) {
    try {
      if (!url) return;
      const result =
        type == CONTRACT_TYPE.ERC1155
          ? await this.getSubgraph1155(url, tokenId)
          : await this.getSubgraph721(url, tokenId);
      return result;
    } catch (error) {
      console.error(error);
    }
  }

  async getSubgraph1155(url: string, tokenId: string) {
    try {
      const client = new GraphQLClient(url);
      const sdk = getSdk1155(client);
      const variables: GetBalances1155QueryVariables = { tokenId: tokenId };
      const reponse = await sdk.getBalances1155(variables);
      return reponse;
    } catch (error) {
      console.error(error);
    }
  }

  async getSubgraph721(url: string, tokenId: string) {
    try {
      const client = new GraphQLClient(url);
      const sdk = getSdk721(client);
      const variables: GetBalances721QueryVariables = { tokenId: tokenId };
      const reponse = await sdk.getBalances721(variables);
      return reponse;
    } catch (error) {
      console.error(error);
    }
  }
}

export default new subgraphServiceCommon();
