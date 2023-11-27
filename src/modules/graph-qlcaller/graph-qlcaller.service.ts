import { Injectable } from '@nestjs/common';
import { CreateGraphQlcallerDto } from './dto/create-graph-qlcaller.dto';
import { UpdateGraphQlcallerDto } from './dto/update-graph-qlcaller.dto';
import {
  getSdk,
  GetNfTsHistory721QueryVariables,
  GetNfTsHistory1155QueryVariables,
  SellStatus,
  GetNfTsHistory721Query,
  GetNfTsHistory1155Query,
  GetOneNftSellInfoQueryVariables,
  GetNftOwnersInfo1155QueryVariables,
  GetNftOwnersInfo721QueryVariables,
  GetNftOwnersInfo721Query,
  GetNftOwnersInfo1155Query,
  GetOneNftSellInfoQuery,
} from '../../generated/graphql';
import { GraphQLClient, gql } from 'graphql-request';
import { CONTRACT_TYPE } from '@prisma/client';
@Injectable()
export class GraphQlcallerService {
  private readonly endpoint = process.env.SUBGRAPH_URL;
  private graphqlClient: GraphQLClient;

  constructor() {
    this.graphqlClient = new GraphQLClient(this.endpoint);
  }

  private getGraphqlClient() {
    return new GraphQLClient(this.endpoint);
  }

  async getNFTsHistory721(minPrice?, maxPrice?, event?: SellStatus) {
    const whereConditions = [];
    const variables = {};

    // Add conditions based on parameters
    if (minPrice !== undefined && minPrice !== null) {
      whereConditions.push('price_gte: $minPrice');
      variables['minPrice'] = minPrice;
    }
    if (maxPrice !== undefined && maxPrice !== null) {
      whereConditions.push('price_lte: $maxPrice');
      variables['maxPrice'] = maxPrice;
    }
    if (event !== undefined && event !== null) {
      whereConditions.push('event: $event');
      variables['event'] = event;
    }

    // Construct the query
    const query = gql`
      query GetNFTsHistory721($minPrice: BigInt, $maxPrice: BigInt, $event: SellStatus) {
        marketEvent721S(
          where: {${whereConditions.join(', ')}}
          orderBy: timestamp
          orderDirection: desc
        ) {
          id
          event
          nftId {
            id
          }
          price
          to
          from
          quoteToken
        }
      }
    `;

    // Execute the query with dynamic variables
    return this.graphqlClient.request(
      query,
      variables,
    ) as unknown as GetNfTsHistory721Query;
  }

  async getNFTsHistory1155(minPrice?, maxPrice?, event?: SellStatus) {
    const whereConditions = [];
    const variables = {};

    // Add conditions based on parameters
    if (minPrice !== undefined && minPrice !== null) {
      whereConditions.push('price_gte: $minPrice');
      variables['minPrice'] = minPrice;
    }
    if (maxPrice !== undefined && maxPrice !== null) {
      whereConditions.push('price_lte: $maxPrice');
      variables['maxPrice'] = maxPrice;
    }
    if (event !== undefined && event !== null) {
      whereConditions.push('event: $event');
      variables['event'] = event;
    }

    // Construct the query
    const query = gql`
      query GetNFTsHistory1155($minPrice: BigInt, $maxPrice: BigInt, $event: SellStatus) {
        marketEvent1155S(
          where: {${whereConditions.join(', ')}}
          orderBy: timestamp
          orderDirection: desc
        ) {
          id
          event
          amounts
          nftId {
            id
          }
          price
          to
          from
          quoteToken
        }
      }
    `;

    // Execute the query with dynamic variables
    return this.graphqlClient.request(
      query,
      variables,
    ) as unknown as GetNfTsHistory1155Query;
  }

  async getNFTSellStatus(
    page: number,
    limit: number,
    nftId?: string,
    from?: string,
    to?: string,
    quoteToken?: string,
    event?: SellStatus,
  ) {
    // const client = this.getGraphqlClient();
    // const sdk = getSdk(client);
    // console.log('let see: ', nftId);
    // const variables: GetOneNftSellInfoQueryVariables = { nftId };
    // const response = sdk.GetOneNFTSellInfo(variables);
    // return response;

    const whereConditions = [];
    const variables = {};
    // Add conditions based on parameters
    if (nftId !== undefined && nftId !== null) {
      whereConditions.push('nftId_contains: $nftId');
      variables['nftId'] = nftId;
    }
    if (from !== undefined && from !== null) {
      whereConditions.push('from: $from');
      variables['from'] = from;
    }
    if (to !== undefined && to !== null) {
      whereConditions.push('to: $to');
      variables['to'] = to;
    }
    if (quoteToken !== undefined && quoteToken !== null) {
      whereConditions.push('quoteToken: $quoteToken');
      variables['quoteToken'] = quoteToken;
    }
    if (event !== undefined && event !== null) {
      whereConditions.push('event: $event');
      variables['event'] = event;
    }
    const isWhereEmpty = Object.keys(whereConditions).length === 0;

    // Construct the query
    const query = gql`
      query GetNFTSellInfo(
        $nftId: String
        $event: SellStatus
        $quoteToken: String
        $buyer: String
        $seller: String
      ) {
        marketEvent1155S(
          ${isWhereEmpty ? '' : `where: {${whereConditions.join(', ')}}`}
          skip: ${page}
          first: ${limit}
          orderBy: timestamp
          orderDirection: desc
        ) {
          id
          event
          nftId {
            id
            contract {
              id
            }
          }
          price
          to
          from
          amounts
          quoteToken
          operationId
        }
        marketEvent721S(
          ${isWhereEmpty ? '' : `where: {${whereConditions.join(', ')}}`}
          orderBy: timestamp
          orderDirection: desc
          skip: $page
          first: $limit
        ) {
          id
          event
          nftId {
            id
            contract {
              id
            }
          }
          price
          to
          from
          quoteToken
        }
      }
    `;

    // Execute the query with dynamic variables
    return this.graphqlClient.request(
      query,
      variables,
    ) as unknown as GetOneNftSellInfoQuery;
  }

  async getOneNFTOwnersInfo1155(
    nftId: string,
  ): Promise<GetNftOwnersInfo1155Query> {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    console.log('let see: ', nftId);
    const variables: GetNftOwnersInfo1155QueryVariables = { nftId };
    const response = sdk.GetNFTOwnersInfo1155(variables);
    return response;
  }

  async getOneNFTOwnersInfo721(
    nftId: string,
  ): Promise<GetNftOwnersInfo721Query> {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    console.log('let see: ', nftId);
    const variables: GetNftOwnersInfo721QueryVariables = { nftId };
    const response = sdk.GetNFTOwnersInfo721(variables);
    return response;
  }
}
