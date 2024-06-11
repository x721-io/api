import { createHash } from 'crypto';
import { encode } from 'punycode';
import { ApiCallerService } from 'src/modules/api-caller/api-caller.service';
import { Prisma, User, MarketplaceStatus } from '@prisma/client';
import { PrismaService } from 'src/prisma/prisma.service';
import {
  Injectable,
  HttpException,
  HttpStatus,
  NotFoundException,
  BadRequestException,
} from '@nestjs/common';
import { NftDto } from '../dto/nft.dto';
import { GetAllNftDto } from '../dto/get-all-nft.dto';
import { nftSelect } from 'src/commons/definitions/Constraint.Object';
import PaginationCommon from 'src/commons/HasNext.common';
import { GraphQlcallerService } from 'src/modules/graph-qlcaller/graph-qlcaller.service';
import { OrderDirection } from 'src/generated/graphql';

interface NFTMarketplaceResponse {
  result: NftDto[];
  hasNext: boolean;
}

@Injectable()
export class NFTHepler {
  constructor(
    private prisma: PrismaService,
    private readonly GraphqlService: GraphQlcallerService,
  ) {}
  weiToEther(wei) {
    return wei / 1000000000000000000; // 1 Ether = 10^18 Wei
  }
  // 1 NFT have multi price => this function return smallest price with quoteToken
  async handleFormatNFTResponse(nfts: any[]) {
    return Promise.all(
      nfts.map(async (item) => {
        if (
          item?.MarketplaceByTokenId &&
          item?.MarketplaceByTokenId.length > 0
        ) {
          const { priceWei, event, quantity, askId, quoteToken } =
            item.MarketplaceByTokenId.reduce(
              (minItem, currentItem) =>
                currentItem.price < minItem.price ? currentItem : minItem,
              item.MarketplaceByTokenId[0],
            );
          const quoteTokenData = await this.getQuoteTokens(quoteToken);
          delete item.MarketplaceByTokenId;
          return {
            ...item,
            price: priceWei,
            sellStatus: event,
            quantity,
            askId,
            quoteToken,
            derivedETH: quoteTokenData?.derivedETH || 0,
            derivedUSD: quoteTokenData?.derivedUSD || 0,
          };
        } else {
          delete item.MarketplaceByTokenId;
          return item;
        }
      }),
    );
  }
  async getQuoteTokens(address: string) {
    try {
      if (!address) {
        return;
      }
      const quoteTokens = await this.prisma.quoteTokens.findUnique({
        where: {
          address: address,
        },
      });
      if (!quoteTokens) {
        return;
      }
      return quoteTokens;
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  formatDataNFTForSort(arr: any[]): NftDto[] {
    return arr.map((item) => {
      const { nftById } = item;
      return {
        ...nftById,
        price: item?.priceWei,
        sellStatus: item?.event,
        quantity: item?.quantity,
        askId: item?.askId,
        quoteToken: item?.quoteToken,
      };
    });
  }

  async getListNFTWithMarketplaceStatus(
    filter: GetAllNftDto,
    whereMarketPlaceStatus: Prisma.MarketplaceStatusWhereInput,
  ): Promise<NFTMarketplaceResponse> {
    const marketplace = await this.prisma.marketplaceStatus.findMany({
      where: whereMarketPlaceStatus,
      skip: (filter.page - 1) * filter.limit,
      take: filter.limit,
      orderBy: [
        {
          metricPoint: 'desc',
        },
        {
          price: filter.order,
        },
      ],
      include: {
        nftById: {
          select: nftSelect,
        },
      },
    });
    const result = this.getSmallestPrices(marketplace);
    const listFormat = await Promise.all(
      result.map(async (item) => {
        const quoteTokenData = await this.getQuoteTokens(item.quoteToken);
        return {
          ...item,
          derivedETH: quoteTokenData?.derivedETH || 0,
          derivedUSD: quoteTokenData?.derivedUSD || 0,
        };
      }),
    );

    const hasNext = await PaginationCommon.hasNextPage(
      filter.page,
      filter.limit,
      'marketplaceStatus',
      whereMarketPlaceStatus,
    );
    return { result: listFormat, hasNext };
  }

  // Get Smallest Price of 1 NFT
  getSmallestPrices(arr: MarketplaceStatus[]): NftDto[] {
    const uniqueCombinationMap = {};
    arr.forEach((item) => {
      const { tokenId, collectionId, quoteToken, price } = item;
      const key = `${tokenId}-${collectionId}-${quoteToken}`;
      if (key in uniqueCombinationMap) {
        if (price < uniqueCombinationMap[key].price) {
          uniqueCombinationMap[key] = { price, item };
        }
      } else {
        uniqueCombinationMap[key] = { price, item };
      }
    });
    const uniqueItems = Object.values(uniqueCombinationMap).map(
      ({ item }) => item,
    );
    return this.formatDataNFTForSort(uniqueItems);
  }

  generateWhereMarketPlaceStatus(
    filter: GetAllNftDto,
  ): Prisma.MarketplaceStatusWhereInput {
    const priceFilter: Prisma.FloatFilter = {};
    const whereMarketPlaceStatus: Prisma.MarketplaceStatusWhereInput = {};
    whereMarketPlaceStatus.AND = [];

    whereMarketPlaceStatus.AND.push({
      quoteToken:
        (filter.quoteToken
          ? filter.quoteToken.toLowerCase()
          : process.env.QUOTE_TOKEN_U2U) ?? process.env.QUOTE_TOKEN_U2U,
    });

    if (filter.priceMin !== undefined || filter.priceMax !== undefined) {
      if (filter.priceMin !== undefined) {
        priceFilter.gte = this.weiToEther(filter.priceMin);
      }
      if (filter.priceMax !== undefined) {
        priceFilter.lte = this.weiToEther(filter.priceMax);
      }
      whereMarketPlaceStatus.AND.push({ price: priceFilter });
    }

    return whereMarketPlaceStatus;
  }

  sortERC1155balances(dataArray, inputOrder = 'asc') {
    const compareTimestamps = (a, b) => a.createAt - b.createAt;

    const sortedArray = dataArray.sort(compareTimestamps);
    if (inputOrder === 'desc') {
      sortedArray.reverse();
    }

    return sortedArray;
  }
}
