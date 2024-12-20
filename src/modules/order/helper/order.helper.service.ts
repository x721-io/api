import { ApiCallerService } from 'src/modules/api-caller/api-caller.service';
import { CONTRACT_TYPE } from '@prisma/client';
import { gql, GraphQLClient } from 'graphql-request';
import {
  getSdk as getSdk1155,
  GetOwner1155QueryVariables,
} from 'src/generated/Template1155/graphql';
import {
  getSdk as getSdk721,
  GetOwner721QueryVariables,
} from 'src/generated/Template721/graphql';
import {
  GetNftOwners1155Query,
  GetNftOwners1155QueryVariables,
  GetNftOwners721Query,
  GetNftOwners721QueryVariables,
  getSdk,
} from 'src/generated/graphql';

class OrderHeplerCommon {
  apiService: ApiCallerService;
  private readonly endpoint = process.env.SUBGRAPH_URL;
  private graphqlClient: GraphQLClient;

  constructor() {
    this.graphqlClient = new GraphQLClient(this.endpoint);
  }

  private getGraphqlClient() {
    return new GraphQLClient(this.endpoint);
  }

  async ownerExternal(
    url: string,
    type: CONTRACT_TYPE,
    tokenId: string,
    owner: string,
  ) {
    try {
      if (!url) return;
      const result =
        type == CONTRACT_TYPE.ERC1155
          ? await this.ownerExternal1155(url, tokenId, owner)
          : await this.ownerExternal721(url, tokenId, owner);
      return result;
    } catch (error) {
      console.error(error);
    }
  }

  async ownerExternal721(url: string, tokenId: string, owner: string) {
    try {
      const client = new GraphQLClient(url);
      const sdk = getSdk721(client);
      const variables: GetOwner721QueryVariables = {
        tokenId: tokenId,
        address: owner,
      };
      const reponse = await sdk.getOwner721(variables);
      return reponse;
    } catch (error) {
      console.error(error);
    }
  }

  async ownerExternal1155(url: string, tokenId: string, owner: string) {
    try {
      const client = new GraphQLClient(url);
      const sdk = getSdk1155(client);
      const variables: GetOwner1155QueryVariables = {
        tokenId: tokenId,
        address: owner,
      };
      const reponse = await sdk.getOwner1155(variables);
      return reponse;
    } catch (error) {
      console.error(error);
    }
  }

  async ownersInternal1155(
    contract: string,
    tokenId: string,
    owner: string,
  ): Promise<GetNftOwners1155Query> {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const variables: GetNftOwners1155QueryVariables = {
      tokenId,
      contract,
      owner,
    };
    const response = await sdk.GetNFTOwners1155(variables);
    return response;
  }

  async ownersInternal721(
    tokenId: string,
    contract: string,
    owner: string,
  ): Promise<GetNftOwners721Query> {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const variables: GetNftOwners721QueryVariables = {
      tokenId,
      contract,
      owner,
    };
    const response = await sdk.GetNFTOwners721(variables);
    return response;
  }

  weiToEther(wei) {
    return wei / 1000000000000000000;
  }
}

export default new OrderHeplerCommon();
