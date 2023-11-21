import { Injectable } from '@nestjs/common';
import { CreateGraphQlcallerDto } from './dto/create-graph-qlcaller.dto';
import { UpdateGraphQlcallerDto } from './dto/update-graph-qlcaller.dto';
import { getSdk, GetCollectionsQueryVariables, GetNfTsHistoryQuery, GetNfTsHistoryQueryVariables } from '../../generated/graphql'
import { GraphQLClient } from 'graphql-request';
@Injectable()
export class GraphQlcallerService {
  private readonly endpoint = process.env.SUBGRAPH_URL;

  private getGraphqlClient() {
    return new GraphQLClient(this.endpoint);
  }
  async getCollections(first: number) {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const variables: GetCollectionsQueryVariables = { first };
    try {
      const response = await sdk.GetCollections(variables);
      return response;
    } catch (err) {
      console.error(err);
      throw err; 
    }
  }
  async getNFTsHistory(id: string) {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const variables: GetNfTsHistoryQueryVariables = { id };
    try {
      const response = await sdk.GetNFTsHistory(variables);
      return response;
    } catch (err) {
      throw err;
    }
  } 
}
