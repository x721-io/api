import { Injectable, HttpException, HttpStatus } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { JwtService } from '@nestjs/jwt';
import { ConfigService } from '@nestjs/config';
import OtherCommon from 'src/commons/Other.common';
import { GetCollectionMarketData } from '../../graph-qlcaller/getCollectionMarketData.service';
import { Prisma, TX_STATUS, CONTRACT_TYPE, User } from '@prisma/client';
import PaginationCommon from 'src/commons/HasNext.common';
import { CollectionEntity } from '../../collection/entities/collection.entity';
import { GetAllCollectionDto } from '../../collection/dto/get-all-collection.dto';
import {
  creatorSelect,
  CollectionSelect,
  marketplaceSelect,
} from '../../../commons/definitions/Constraint.Object';
import { GetAllUser } from '../../user/dto/get-all-user.dto';
import { GraphQlcallerService } from '../../graph-qlcaller/graph-qlcaller.service';
import { NftDto } from '../../nft/dto/nft.dto';
import { GetAllNftDto } from '../../nft/dto/get-all-nft.dto';
import { OrderDirection, SellStatus } from 'src/generated/graphql';
import { NftService } from '../../nft/nft.service';
import { NFTHepler } from '../../nft/helper/nft-helper.service';
interface CollectionGeneral {
  totalOwner: number;
  volumn: string;
  totalNft: number;
  // floorPrice: bigint;
}
@Injectable()
export class MarketplaceCMSService {
  constructor(
    private readonly configService: ConfigService,
    private readonly prisma: PrismaService,
    private jwtService: JwtService,
    private readonly collectionData: GetCollectionMarketData,
    private readonly GraphqlService: GraphQlcallerService,
    private readonly nftService: NftService,
    private nftHepler: NFTHepler,
  ) {}

  async findAllCollection(
    input: GetAllCollectionDto,
  ): Promise<PagingResponseHasNext<CollectionEntity>> {
    // TODO: get all collection from subgraph first, got the id and map it back to local collection
    const creators = input.creatorAddresses
      ? await this.prisma.user.findMany({
          where: {
            publicKey: {
              in: input.creatorAddresses,
              mode: 'insensitive',
            },
          },
        })
      : [];
    const minBigInt = input.min
      ? BigInt(input.min) / BigInt(10) ** 18n
      : undefined;
    const maxBigInt = input.max
      ? BigInt(input.max) / BigInt(10) ** 18n
      : undefined;
    const addresses = creators.map((item) => item.id);
    let whereCondition: Prisma.CollectionWhereInput = {
      ...(input.name && {
        nameSlug: {
          contains: OtherCommon.stringToSlugSearch(input.name),
          mode: 'insensitive',
        },
      }),
      ...(addresses.length > 0 && {
        creators: {
          some: {
            userId: {
              in: addresses,
            },
          },
        },
      }),
      status: TX_STATUS.SUCCESS,
    };
    if (input.min) {
      whereCondition = {
        ...whereCondition,
        floorPrice: {
          gte: minBigInt,
        },
      };
    }

    if (input.max) {
      whereCondition = {
        ...whereCondition,
        floorPrice: {
          lte: maxBigInt,
        },
      };
    }

    if (input.min && input.max) {
      whereCondition = {
        ...whereCondition,
        floorPrice: {
          gte: minBigInt,
          lte: maxBigInt,
        },
      };
    }

    if (input.max && input.min && BigInt(input.min) > BigInt(input.max)) {
      return {
        data: [],
        paging: {
          hasNext: false,
          page: input.page,
          limit: input.limit,
        },
      };
    }
    const orderByProperties: Prisma.CollectionOrderByWithRelationAndSearchRelevanceInput =
      {};
    if (input.orderBy == 'time') {
      orderByProperties.createdAt = input.order;
    } else {
      orderByProperties.floorPrice = input.order;
    }

    const collections = await this.prisma.collection.findMany({
      where: whereCondition,
      skip: (input.page - 1) * input.limit,
      take: input.limit,
      orderBy: orderByProperties,
      include: {
        creators: {
          select: {
            userId: true,
            user: {
              select: creatorSelect,
            },
          },
        },
      },
    });
    // const subgraphCollection = collections.map(async (item) => {
    //   const generalInfo = await this.getGeneralCollectionData(
    //     item.address,
    //     item.type,
    //   );
    //   return { ...item, ...generalInfo };
    // });
    // const dataArray = Promise.all(subgraphCollection);

    const dataArray = await Promise.all(
      collections.map(async (item) => {
        const generalInfo = await this.getGeneralCollectionData(
          item.address,
          item.type,
        );
        return { ...item, ...generalInfo };
      }),
    );
    const hasNext = await PaginationCommon.hasNextPage(
      input.page,
      input.limit,
      'collection',
      whereCondition,
    );
    return {
      data: await dataArray,
      paging: {
        hasNext,
        limit: input.limit,
        page: input.page,
      },
    };
    // }
  }
  async getGeneralCollectionData(
    collectionAddress: string,
    type: CONTRACT_TYPE,
  ): Promise<CollectionGeneral> {
    if (!collectionAddress) {
      return {
        volumn: '0',
        totalOwner: Number(0),
        totalNft: Number(0),
        // floorPrice: BigInt(0),
      };
    }
    const [statusCollection] = await Promise.all([
      this.collectionData.getCollectionCount(collectionAddress),
      // this.getVolumeCollection(collectionAddress),
    ]);

    if (type === 'ERC721') {
      return {
        // volumn: sum.toString(),
        volumn: statusCollection.erc721Contract?.volume || 0,
        totalOwner: statusCollection.erc721Contract?.holderCount || 0,
        totalNft: statusCollection.erc721Contract?.count || 0,
        // floorPrice: BigInt(0),
      };
    } else {
      return {
        // volumn: sum.toString(),
        volumn: statusCollection.erc1155Contract?.volume || 0,
        totalOwner: statusCollection.erc1155Contract?.holderCount || 0,
        totalNft: statusCollection.erc1155Contract?.count || 0,
        // floorPrice: BigInt(0),
      };
    }
  }

  async findAllUser(filter: GetAllUser): Promise<PagingResponseHasNext<any>> {
    // const limit = (filter.limit || 12) as number;
    // const cursor = filter.cursor;
    // @ts-ignore
    // const take: number = limit && limit > 0 ? parseInt(limit) + 1 : 13;
    const whereCondition: any = {
      ...(filter.search
        ? {
            OR: [
              {
                username: {
                  contains: filter.search,
                  mode: 'insensitive',
                },
              },
              {
                email: {
                  contains: filter.search,
                  mode: 'insensitive',
                },
              },
              {
                signer: {
                  contains: filter.search,
                  mode: 'insensitive',
                },
              },
              {
                shortLink: {
                  contains: filter.search,
                  mode: 'insensitive',
                },
              },
              // Add more fields as needed
            ],
          }
        : {}),
      username: {
        not: null,
      },
    };

    const usersWithFollowStatus = await this.prisma.user.findMany({
      orderBy: {
        createdAt: filter.order,
      },
      where: whereCondition,
      skip: (filter.page - 1) * filter.limit,
      take: filter.limit,
      select: {
        id: true,
        email: true,
        avatar: true,
        username: true,
        signature: true,
        signedMessage: true,
        signDate: true,
        signer: true,
        publicKey: true,
        acceptedTerms: true,
        createdAt: true,
        updatedAt: true,
        bio: true,
        facebookLink: true,
        twitterLink: true,
        telegramLink: true,
        shortLink: true,
        discordLink: true,
        webURL: true,
        coverImage: true,
        followers: true,
        following: true,
        accountStatus: true,
        verifyEmail: true,
        isActive: true,
      },
    });
    const hasNext = await PaginationCommon.hasNextPage(
      filter.page,
      filter.limit,
      'user',
      whereCondition,
    );
    return {
      data: usersWithFollowStatus,
      paging: {
        hasNext,
        page: filter.page,
        limit: filter.limit,
      },
    };
  }

  async findAllNFT(
    filter: GetAllNftDto,
  ): Promise<PagingResponseHasNext<NftDto>> {
    // TODO: Reimplement pagination strategy
    // Get the result totally from subgraph and match data to local storage
    // For each set of condition, use different subgraph query as source
    // owner: getNFTFromOwner
    // owner + sellStatus + priceMax + priceMin + collectionType: marketplace721S and marketplace1155S
    try {
      let traitsConditions = [];

      // TODO: if price and status are included, then use subgraph as main source and use other to eliminate
      if (filter.traits) {
        traitsConditions = filter.traits.map((trait) => ({
          traits: {
            some: {
              trait_type: trait.trait_type,
              ...(trait.value && { value: trait.value }),
              ...(trait.display_type && { display_type: trait.display_type }),
            },
          },
        }));
      }
      let nftIdFromOwner = [];
      let nftCollectionFromOwner = [];
      let hasNextNftOwner = false;
      if (filter.owner) {
        // External
        const resultOwnerExternal =
          await this.GraphqlService.getNFTExternalFromOwner(
            filter.owner.toLocaleLowerCase(),
            filter.order as OrderDirection,
            filter.page,
            Math.floor(filter.limit / 2),
          );

        const hasNextNftOwnerExternalTemp =
          await this.GraphqlService.getNFTExternalFromOwner(
            filter.owner.toLocaleLowerCase(),
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
          const erc1155BalancesSort = this.nftHepler.sortERC1155balances(
            resultOwnerExternal.erc1155Balances,
            filter.order,
          );

          const nftIdFromOwnerExternal = resultOwnerExternal.erc721Tokens
            .map((item) => item.tokenID)
            .concat(erc1155BalancesSort.map((item) => item.token.tokenID));
          const nftCollectionFromOwnerExternal =
            resultOwnerExternal.erc721Tokens
              .map((item) => item.contract)
              .concat(erc1155BalancesSort.map((item) => item.token.contract));

          nftIdFromOwner = [...nftIdFromOwnerExternal];
          nftCollectionFromOwner = [...nftCollectionFromOwnerExternal];
        }
        // Check if the number of external items is less than the limit
        if (nftIdFromOwner?.length < filter.limit) {
          // Internal
          const limitRemaining = filter.limit - (nftIdFromOwner?.length || 0);

          const { account } = await this.GraphqlService.getNFTFromOwner(
            filter.owner.toLocaleLowerCase(),
            filter.order as OrderDirection,
            filter.page,
            Math.floor(limitRemaining / 2),
          );
          const { account: hasNextNftOwnerTemp } =
            await this.GraphqlService.getNFTFromOwner(
              filter.owner.toLocaleLowerCase(),
              filter.order as OrderDirection,
              filter.page + 1,
              Math.floor(limitRemaining / 2),
            );
          hasNextNftOwner =
            hasNextNftOwner ||
            (hasNextNftOwnerTemp &&
              hasNextNftOwnerTemp.ERC721tokens.length > 0) ||
            (hasNextNftOwnerTemp &&
              hasNextNftOwnerTemp.ERC1155balances.length > 0);
          if (account) {
            const erc1155BalancesSort = this.nftHepler.sortERC1155balances(
              account.ERC1155balances,
              filter.order,
            );
            const nftIdFromOwnerInternal = account.ERC721tokens.map(
              (item) => item.tokenId,
            ).concat(erc1155BalancesSort.map((item) => item.token.tokenId));
            const nftCollectionFromOwnerInternal = account.ERC721tokens.map(
              (item) => item.contract.id,
            ).concat(erc1155BalancesSort.map((item) => item.token.contract.id));
            nftIdFromOwner = [...nftIdFromOwner, ...nftIdFromOwnerInternal];
            nftCollectionFromOwner = [
              ...nftCollectionFromOwner,
              ...nftCollectionFromOwnerInternal,
            ];
          }
        }
      }
      const whereCondition: Prisma.NFTWhereInput = {};
      const whereConditionInternal: Prisma.NFTWhereInput = {};
      whereConditionInternal.AND = [];
      whereCondition.OR = [];

      // Handle traits conditions
      if (traitsConditions.length > 0) {
        whereConditionInternal.AND.push(...traitsConditions);
      }

      whereConditionInternal.AND.push({
        status: TX_STATUS.SUCCESS,
      });

      whereConditionInternal.AND.push({
        isActive: true,
      });
      if (filter.creatorAddress) {
        whereConditionInternal.AND.push({
          creator: {
            publicKey: filter.creatorAddress,
          },
        });
      }

      if (filter.collectionAddress || filter.type) {
        const collectionCondition: Prisma.CollectionWhereInput = {};

        if (filter.collectionAddress) {
          collectionCondition.address = filter.collectionAddress;
        }

        if (filter.type) {
          collectionCondition.type = filter.type;
        }

        whereConditionInternal.AND.push({ collection: collectionCondition });
      }

      if (filter.name) {
        whereConditionInternal.AND.push({
          // name: {
          //   contains: filter.name,
          //   mode: 'insensitive',
          // },
          nameSlug: {
            contains: OtherCommon.stringToSlugSearch(filter.name),
            mode: 'insensitive',
          },
        });
      }

      if (nftIdFromOwner.length > 0) {
        const collectionToTokenIds: Record<string, string[]> = {};
        for (let i = 0; i < nftIdFromOwner.length; i++) {
          const collection = nftCollectionFromOwner[i];
          if (!collectionToTokenIds[collection]) {
            collectionToTokenIds[collection] = [];
          }
          collectionToTokenIds[collection].push(nftIdFromOwner[i]);
        }
        for (const [collection, tokenIds] of Object.entries(
          collectionToTokenIds,
        )) {
          const tokenIdConditions = tokenIds.map((tokenId) => ({
            OR: [{ u2uId: tokenId }, { id: tokenId }],
          }));
          whereCondition.OR.push({
            AND: [
              { OR: tokenIdConditions },
              {
                collection: {
                  address: collection,
                },
              },
              ...whereConditionInternal.AND,
            ],
          });
        }
      } else if (filter.owner) {
        // console.log(whereConditionInternal);
      } else {
        whereCondition.AND = whereConditionInternal.AND;
        delete whereCondition.OR;
      }
      //----------
      if (
        (!filter.priceMin && !filter.priceMax && !filter.sellStatus) ||
        filter.name
      ) {
        if (filter.quoteToken !== undefined) {
          whereCondition.MarketplaceByTokenId = { some: {} };
          whereCondition.MarketplaceByTokenId.some.quoteToken =
            filter.quoteToken.toLowerCase();
        }
        const whereMarketPlaceStatus: Prisma.MarketplaceStatusWhereInput =
          this.nftHepler.generateWhereMarketPlaceStatus(filter);

        if (filter.orderBy === 'price') {
          whereMarketPlaceStatus.nftById = whereCondition;
          const { result, hasNext } =
            await this.nftHepler.getListNFTWithMarketplaceStatus(
              filter,
              whereMarketPlaceStatus,
            );
          return {
            data: result,
            paging: {
              hasNext: hasNext,
              limit: filter.limit,
              page: filter.page,
            },
          };
        } else {
          const orderByProperties: Prisma.NFTOrderByWithRelationAndSearchRelevanceInput =
            {};

          if (filter.orderBy == 'time') {
            orderByProperties.createdAt = filter.order;
          } else {
            orderByProperties.metricPoint = 'desc';
          }
          const nfts = await this.prisma.nFT.findMany({
            ...(!filter.owner && {
              skip: (filter.page - 1) * filter.limit,
              take: filter.limit,
            }),
            where: whereCondition,
            orderBy: orderByProperties,
            include: {
              creator: {
                select: creatorSelect,
              },
              collection: {
                select: CollectionSelect,
              },
              MarketplaceByTokenId: {
                where: whereMarketPlaceStatus,
                select: marketplaceSelect,
              },
              traits: true,
            },
          });
          const Nftformat = await this.nftHepler.handleFormatNFTResponse(nfts);
          const hasNext =
            (await PaginationCommon.hasNextPage(
              filter.page,
              filter.limit,
              'nFT',
              whereCondition,
            )) || hasNextNftOwner;
          return {
            data: Nftformat,
            paging: {
              hasNext: hasNext,
              limit: filter.limit,
              page: filter.page,
            },
          };
        }
      } else {
        if (Number(filter.priceMin) > Number(filter.priceMax)) {
          // If priceMin is higher than priceMax, return an empty array
          return {
            data: [],
            paging: {
              hasNext: false,
              limit: filter.limit,
              page: filter.page,
            },
          };
        }
        const whereMarketPlaceStatus: Prisma.MarketplaceStatusWhereInput =
          this.nftHepler.generateWhereMarketPlaceStatus(filter);
        const whereCondition1: Prisma.NFTWhereInput = {
          AND: [whereCondition],
        };
        // Ensure that MarketplaceByTokenId is initialized
        if (!whereCondition1.MarketplaceByTokenId) {
          whereCondition1.MarketplaceByTokenId = { some: {} };
        }

        if (filter.priceMin !== undefined || filter.priceMax !== undefined) {
          whereCondition1.MarketplaceByTokenId.some.price = {};
          if (filter.priceMin !== undefined) {
            whereCondition1.MarketplaceByTokenId.some.price.gte = Number(
              OtherCommon.weiToEther(filter.priceMin),
            );
          }
          if (filter.priceMax !== undefined) {
            whereCondition1.MarketplaceByTokenId.some.price.lte = Number(
              OtherCommon.weiToEther(filter.priceMax),
            );
          }
        }
        // Check if filter.from or filter.quoteToken is defined before adding it to the query
        if (filter.from !== undefined || filter.owner !== undefined) {
          whereCondition1.MarketplaceByTokenId.some.from =
            filter.sellStatus === SellStatus.AskNew && filter.owner
              ? filter.owner.toLowerCase()
              : filter.from;
        }

        whereCondition1.MarketplaceByTokenId.some.quoteToken =
          (filter.quoteToken
            ? filter.quoteToken.toLowerCase()
            : process.env.QUOTE_TOKEN_U2U) ?? process.env.QUOTE_TOKEN_U2U;

        if (filter.orderBy === 'price') {
          whereMarketPlaceStatus.nftById = whereCondition1;
          const { result, hasNext } =
            await this.nftHepler.getListNFTWithMarketplaceStatus(
              filter,
              whereMarketPlaceStatus,
            );
          return {
            data: result,
            paging: {
              hasNext: hasNext,
              limit: filter.limit,
              page: filter.page,
            },
          };
        } else {
          const orderByProperties: Prisma.NFTOrderByWithRelationAndSearchRelevanceInput =
            {};

          if (filter.orderBy == 'time') {
            orderByProperties.createdAt = filter.order;
          } else {
            orderByProperties.metricPoint = 'desc';
          }

          const nfts = await this.prisma.nFT.findMany({
            ...(!filter.owner && {
              skip: (filter.page - 1) * filter.limit,
              take: filter.limit,
            }),
            where: whereCondition1,
            orderBy: orderByProperties,
            include: {
              creator: {
                select: creatorSelect,
              },
              collection: {
                select: CollectionSelect,
              },
              MarketplaceByTokenId: {
                where: whereMarketPlaceStatus,
                select: marketplaceSelect,
              },
              traits: true,
            },
          });
          const Nftformat = await this.nftHepler.handleFormatNFTResponse(nfts);
          const hasNext = await PaginationCommon.hasNextPage(
            filter.page,
            filter.limit,
            'nFT',
            whereCondition1,
          );
          return {
            data: Nftformat,
            paging: {
              hasNext: hasNext,
              limit: filter.limit,
              page: filter.page,
            },
          };
        }
      }
    } catch (error) {
      console.error(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
}
