import { createHash } from 'crypto';
import { encode } from 'punycode';
import { ApiCallerService } from 'src/modules/api-caller/api-caller.service';
import { Prisma, User, MarketplaceStatus, TX_STATUS } from '@prisma/client';
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
import { validate as isValidUUID } from 'uuid';

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
          price: filter.order,
        },
        {
          metricPoint: 'desc',
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

  reduceData721(data: any[]) {
    const uniqueData = Array.from(
      data
        .reduce((map, item) => {
          const key = item?.id;
          if (!map.has(key)) {
            map.set(key, item);
          }

          return map;
        }, new Map())
        .values(),
    );
    return uniqueData;
  }

  reduceData1155(data: any[]) {
    const uniqueData = Array.from(
      data
        .reduce((map, item) => {
          const key = item?.token?.id;

          if (!map.has(key)) {
            map.set(key, item);
          }

          return map;
        }, new Map())
        .values(),
    );
    return uniqueData;
  }
  async handleGetOwnerNFT(filter: GetAllNftDto | any) {
    try {
      let nftIdFromOwner = [];
      let nftCollectionFromOwner = [];
      let hasNextNftOwner = false;
      const resultOwnerExternal =
        await this.GraphqlService.getNFTExternalFromOwner(
          filter.owner.toLowerCase(),
          filter.order as OrderDirection,
          filter.page,
          Math.floor(filter.limit / 2),
        );

      const hasNextNftOwnerExternalTemp =
        await this.GraphqlService.getNFTExternalFromOwner(
          filter.owner.toLowerCase(),
          filter.order as OrderDirection,
          filter.page + 1,
          Math.floor(filter.limit / 2),
        );

      hasNextNftOwner =
        (hasNextNftOwnerExternalTemp &&
          hasNextNftOwnerExternalTemp.erc721Tokens.length > 0) ||
        (hasNextNftOwnerExternalTemp &&
          hasNextNftOwnerExternalTemp.erc1155Balances.length > 0);

      if (resultOwnerExternal) {
        const erc1155BalancesSort = this.sortERC1155balances(
          resultOwnerExternal.erc1155Balances,
          filter.order,
        );

        const nftIdFromOwnerExternal = resultOwnerExternal.erc721Tokens
          .map((item) => item.tokenID)
          .concat(erc1155BalancesSort.map((item) => item.token.tokenID));
        const nftCollectionFromOwnerExternal = resultOwnerExternal.erc721Tokens
          .map((item) => item.contract)
          .concat(erc1155BalancesSort.map((item) => item.token.contract));

        nftIdFromOwner = [...nftIdFromOwnerExternal];
        nftCollectionFromOwner = [...nftCollectionFromOwnerExternal];
      }
      // Check if the number of external items is less than the limit
      if (nftIdFromOwner?.length < Math.floor(filter.limit / 2)) {
        // Internal
        // const limitRemaining = filter.limit - (nftIdFromOwner?.length || 0);

        const { account } = await this.GraphqlService.getNFTFromOwner(
          filter.owner.toLowerCase(),
          filter.order as OrderDirection,
          filter.page,
          // filter.limit,
          Math.floor(filter.limit / 2),
        );
        const { account: hasNextNftOwnerTemp } =
          await this.GraphqlService.getNFTFromOwner(
            filter.owner.toLowerCase(),
            filter.order as OrderDirection,
            filter.page + 1,
            // filter.limit,
            Math.floor(filter.limit / 2),
          );

        if (hasNextNftOwnerTemp) {
          const hasNext721Exist = await this.filterExistingNFTs(
            hasNextNftOwnerTemp?.ERC721tokens,
            (item) => item?.tokenId,
            (item) => item?.contract?.id,
            false,
          );

          const hasNext1155Exist = await this.filterExistingNFTs(
            hasNextNftOwnerTemp?.ERC1155balances,
            (item) => item?.tokenId,
            (item) => item?.contract?.id,
            false,
          );

          hasNextNftOwner =
            hasNextNftOwner ||
            hasNext1155Exist?.length > 0 ||
            hasNext721Exist?.length > 0;
        }

        if (account) {
          // Lọc các record fake của external 721 được ghi vào subgraph chính
          const internal721Filter = await this.filterExistingNFTs(
            account?.ERC721tokens,
            (item) => item?.tokenId,
            (item) => item?.contract?.id,
            false,
          );

          // Lọc các record fake của external 1155 được ghi vào subgraph chính
          const internal1155Filter = await this.filterExistingNFTs(
            account?.ERC1155balances,
            (item) => item?.token?.tokenId,
            (item) => item?.token?.contract?.id,
            false,
          );

          const erc1155BalancesSort = this.sortERC1155balances(
            internal1155Filter,
            filter.order,
          );

          // Concat list TokenId
          const nftIdFromOwnerInternal = internal721Filter
            .map((item) => item.tokenId)
            .concat(erc1155BalancesSort.map((item) => item.token.tokenId));

          // Concat List Collection
          const nftCollectionFromOwnerInternal = internal721Filter
            .map((item) => item.contract.id)
            .concat(erc1155BalancesSort.map((item) => item.token.contract.id));
          nftIdFromOwner = [...nftIdFromOwner, ...nftIdFromOwnerInternal];
          nftCollectionFromOwner = [
            ...nftCollectionFromOwner,
            ...nftCollectionFromOwnerInternal,
          ];
        }
      }
      return {
        nftIdFromOwner,
        nftCollectionFromOwner,
        hasNextNftOwner,
      };
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
  async filterExistingNFTs(
    items: any[],
    getTokenId: (item: any) => string,
    getContractId: (item: any) => string,
    external?: boolean,
  ) {
    if (items && items.length <= 0) {
      return [];
    }
    const existsItems = await Promise.all(
      items.map(async (item) => {
        const exists = await this.checkExistNFT(
          getTokenId(item),
          getContractId(item),
          external,
        );
        return { item, exists };
      }),
    );
    return existsItems.filter(({ exists }) => exists).map(({ item }) => item);
  }

  async checkExistNFT(
    tokenId: string,
    addressCollection: string,
    external?: boolean,
  ) {
    try {
      const whereCondition: Prisma.CollectionWhereInput = {};
      whereCondition.AND = [];
      if (isValidUUID(addressCollection)) {
        whereCondition.AND.push({ id: addressCollection });
      } else {
        whereCondition.AND.push({ address: addressCollection });
      }
      if (external == true || external == false) {
        whereCondition.AND.push({ flagExtend: external });
      }

      if (!addressCollection) return false;
      const collection = await this.prisma.collection.findFirst({
        where: {
          ...whereCondition,
          status: TX_STATUS.SUCCESS,
        },
      });
      if (!collection) return false;
      const nftExists = await this.prisma.nFT.findFirst({
        where: {
          collectionId: collection.id,
          OR: [{ id: tokenId }, { u2uId: tokenId }],
          status: TX_STATUS.SUCCESS,
          isActive: true,
        },
      });
      return !!nftExists;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
}
