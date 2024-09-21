import { Injectable, HttpException, HttpStatus } from '@nestjs/common';
import {
  getSdk,
  SellStatus,
  GetNfTsHistory721Query,
  GetNfTsHistory1155Query,
  GetNftOwnersInfo1155QueryVariables,
  GetNftOwnersInfo721QueryVariables,
  GetNftOwnersInfo721Query,
  GetNftOwnersInfo1155Query,
  GetOneNftSellInfoQuery,
  GetNfTwithAccountIdQueryVariables,
  GetCollectionTokensQueryVariables,
  Query,
  OrderDirection,
  CmsSummaryTransactionQueryVariables,
  EventType,
  GetActivityWithEventQueryVariables,
} from '../../generated/graphql';

import { getSdk as getSdkExternal } from '../../generated/SubgraphExternal/graphql';

import { GraphQLClient, gql } from 'graphql-request';
import { GetCheckOwnerExternalQueryVariables } from 'src/generated/SubgraphExternal/graphql';
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

  async getNFTsHistory721(
    minPrice?,
    maxPrice?,
    event?: SellStatus,
    quoteToken?: string,
  ) {
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
    if (quoteToken !== undefined && quoteToken !== null) {
      whereConditions.push('quoteToken: $quoteToken');
      variables['quoteToken'] = quoteToken;
    }

    // Construct the query
    const query = gql`
      query GetNFTsHistory721($minPrice: BigInt, $maxPrice: BigInt, $event: SellStatus, $quoteToken: String) {
        marketEvent721S(
          where: {${whereConditions.join(', ')}}
          orderBy: timestamp
          orderDirection: desc
        ) {
          id
          event
          nftId {
            id
            tokenId
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
    ) as unknown as GetNfTsHistory721Query;
  }

  async getNFTsHistory1155(
    minPrice?,
    maxPrice?,
    event?: SellStatus,
    quoteToken?: string,
  ) {
    const whereConditions = [];
    const variables = {};

    // Add conditions based on parameters
    if (minPrice !== undefined && minPrice !== null) {
      whereConditions.push('price_gte: $minPrice');
      variables['minPrice'] = minPrice;
    }
    if (quoteToken !== undefined && quoteToken !== null) {
      whereConditions.push('quoteToken: $quoteToken');
      variables['quoteToken'] = quoteToken;
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
      query GetNFTsHistory1155($minPrice: BigInt, $maxPrice: BigInt, $event: SellStatus, $quoteToken: String) {
        marketEvent1155S(
          where: {${whereConditions.join(', ')}}
          orderBy: timestamp
          orderDirection: desc
        ) {
          id
          event
          quantity
          nftId {
            id
            tokenId
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
    ) as unknown as GetNfTsHistory1155Query;
  }

  async getNFTSellStatus1(
    conditions: { or?: any[]; and?: any[] },
    page?: number,
    limit?: number,
  ) {
    const { or, and } = conditions;
    // console.log('and: ', conditions);
    const processCondition = (condition: any): string => {
      return Object.entries(condition)
        .filter(([key, value]) => !!value) // Filter out null values
        .map(([key, value]) => {
          if (typeof value === 'object' && value !== null) {
            // Process nested object
            return `${key}: {${processCondition(value)}}`;
          } else {
            // Process simple key-value pair
            return `${key}: "${value}"`;
          }
        })
        .join(', ');
    };

    const whereParts = [];

    // Process AND conditions
    if (and && and.length > 0) {
      const andConditions = and
        .map(processCondition)
        .filter((condition) => condition) // Filter out empty conditions
        .map((condition) => `{${condition}}`);

      if (andConditions.length > 0) {
        whereParts.push(`and: [${andConditions.join(', ')}]`);
      }
    }

    // Process OR conditions
    if (or && or.length > 0) {
      const orConditions = or
        .map(processCondition)
        .filter((condition) => condition) // Filter out empty conditions
        .map((condition) => `{${condition}}`);

      if (orConditions.length > 0) {
        whereParts.push(`or: [${orConditions.join(', ')}]`);
      }
    }

    const isWhereEmpty = whereParts.length === 0;
    const whereClause =
      whereParts.length > 0 ? `where: {${whereParts.join(', ')}}` : '';

    const query = gql`
      query GetNFTSellInfo($page: Int, $limit: Int) {
        marketEvent1155S(
          ${isWhereEmpty ? '' : whereClause}
          skip: $page
          first: $limit
          orderBy: timestamp
          orderDirection: desc
        ) {
          id
          event
          nftId {
            id
            tokenId
            contract {
              id
              name
            }
          }
          price
          to
          from
          quantity
          quoteToken
          operationId
          timestamp
        }
        marketEvent721S(
          ${isWhereEmpty ? '' : whereClause}
          orderBy: timestamp
          orderDirection: desc
          skip: $page
          first: $limit
        ) {
          id
          event
          nftId {
            id
            tokenId
            contract {
              id
              name
            }
          }
          price
          to
          from
          quoteToken
          timestamp
        }
      }
    `;

    const pageCalculation = (page - 1) * limit;
    const response = (await this.graphqlClient.request(query, {
      page: pageCalculation,
      limit,
    })) as unknown as GetOneNftSellInfoQuery;
    const responseHasNext = (await this.graphqlClient.request(query, {
      page: pageCalculation,
      limit: limit * 2,
    })) as unknown as GetOneNftSellInfoQuery;
    const hasNextPage1155 = responseHasNext.marketEvent1155S.length > limit;
    const hasNextPage721 = responseHasNext.marketEvent721S.length > limit;
    return {
      ...response,
      hasNextSubGraph: hasNextPage1155 || hasNextPage721,
    };
  }

  async getNFTSellStatus(
    page: number,
    limit: number,
    contract: string,
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
    if (
      nftId !== undefined &&
      nftId !== null &&
      contract !== undefined &&
      contract !== null
    ) {
      // whereConditions.push('nftId_: {tokenId: $nftId, contract: $contract}');
      whereConditions.push('tokenId: $nftId, address: $contract');
      variables['nftId'] = nftId;
      variables['contract'] = contract;
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
    console.log('where: ', whereConditions.join(', '));
    // Construct the query
    const query = gql`
      query GetNFTSellInfo(
        $nftId: String
        $event: SellStatus
        $quoteToken: String
        $to: String
        $from: String
        $contract: String
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
            tokenId
            contract {
              id
              name
            }
          }
          price
          to
          from
          quantity
          quoteToken
          operationId
          timestamp
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
            tokenId
            contract {
              id
              name
            }
          }
          price
          to
          from
          quoteToken
          timestamp
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
    contractAddress: string,
    nftId: string,
  ): Promise<GetNftOwnersInfo1155Query> {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    // const id = OtherCommon.generateCombineKey([contractAddress, nftId]);
    // console.log('let see: ', id);
    const variables: GetNftOwnersInfo1155QueryVariables = {
      nftId,
      contractAddress,
    };
    const response = sdk.GetNFTOwnersInfo1155(variables);
    return response;
  }

  async getOneNFTOwnersInfo721(
    contractAddress: string,
    nftId: string,
  ): Promise<GetNftOwnersInfo721Query> {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    // const id = OtherCommon.generateCombineKey([contractAddress, nftId]);
    // console.log('let see: ', id);
    const variables: GetNftOwnersInfo721QueryVariables = {
      nftId,
      contractAddress,
    };
    const response = sdk.GetNFTOwnersInfo721(variables);
    return response;
  }

  async getNFTFromOwner(
    owner: string,
    orderDirection: OrderDirection,
    page?: number,
    limit?: number,
  ) {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const variables: GetNfTwithAccountIdQueryVariables = {
      id: owner,
      orderDirection: orderDirection,
      limit: limit > 0 ? limit : 1,
      page: (page - 1) * limit,
    };
    const response = sdk.getNFTwithAccountID(variables);
    return response;
  }
  async getNFTFromCollection(contractAddress: string) {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const variables: GetCollectionTokensQueryVariables = {
      collectionAddress: contractAddress,
    };
    const response = sdk.GetCollectionTokens(variables);
    return response;
  }
  async formatWherecondition(conditions: { or?: any[]; and?: any[] }) {
    const { or, and } = conditions;

    const processCondition = (condition: any): string => {
      return Object.entries(condition)
        .filter(([key, value]) => !!value) // Filter out null values
        .map(([key, value]) => {
          if (typeof value === 'object' && value !== null) {
            // Process nested object
            return `${key}: {${processCondition(value)}}`;
          } else {
            // Process simple key-value pair
            return `${key}: "${value}"`;
          }
        })
        .join(', ');
    };

    const whereParts = [];

    // Process AND conditions
    if (and && and.length > 0) {
      const andConditions = and
        .map(processCondition)
        .filter((condition) => condition) // Filter out empty conditions
        .map((condition) => `{${condition}}`);

      if (andConditions.length > 0) {
        whereParts.push(`and: [${andConditions.join(', ')}]`);
      }
    }

    // Process OR conditions
    if (or && or.length > 0) {
      const orConditions = or
        .map(processCondition)
        .filter((condition) => condition) // Filter out empty conditions
        .map((condition) => `{${condition}}`);

      if (orConditions.length > 0) {
        whereParts.push(`or: [${orConditions.join(', ')}]`);
      }
    }

    const isWhereEmpty = whereParts.length === 0;
    const whereClause =
      whereParts.length > 0 ? `where: {${whereParts.join(', ')}}` : '';
    return { isWhereEmpty, whereClause };
  }

  async FetchRoyaltiesFromGraph(address: string) {
    try {
      const client = this.getGraphqlClient();
      const sdk = getSdk(client);
      const { royaltiesRegistries } = (await sdk.getRoyalties({
        address,
      })) as unknown as Query;
      return royaltiesRegistries;
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
  async getNFTOnSalesAndOwner(address: string) {
    try {
      const client = this.getGraphqlClient();
      const sdk = getSdk(client);
      const { account } = (await sdk.getNFTOnSalesAndOwner({
        id: address,
      })) as unknown as Query;
      return account;
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getNFTExternalFromOwner(
    owner: string,
    orderDirection: OrderDirection,
    page?: number,
    limit?: number,
  ) {
    const client = new GraphQLClient(
      process.env.SUBGRAPH_EXTERNAL_URL as string,
    );
    const sdk = getSdkExternal(client);
    const variables: GetCheckOwnerExternalQueryVariables = {
      owner: owner,
      orderDirection: orderDirection,
      limit: limit > 0 ? limit : 1,
      page: (page - 1) * limit,
    };
    const response = sdk.getCheckOwnerExternal(variables);
    return response;
  }

  async getSummaryTransaction(
    event?: EventType,
    skip?: number,
    first?: number,
    start?: number,
    end?: number,
  ) {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const variables: CmsSummaryTransactionQueryVariables = {
      event: event,
      start: start,
      end: end,
      first,
      skip,
    };
    const response = sdk.CMSSummaryTransaction(variables);
    return response;
  }

  async getSummaryVolume(address: string) {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const response = await sdk.CMSSummaryVolume({ address: address });
    return response;
  }

  async getListActivity(
    event?: EventType,
    address?: string,
    skip?: number,
    first?: number,
  ) {
    const client = this.getGraphqlClient();
    const sdk = getSdk(client);
    const variables: GetActivityWithEventQueryVariables = {
      address,
      event: event,
      first,
      skip,
    };
    const response = await sdk.getActivityWithEvent(variables);
    return response;
  }
}
