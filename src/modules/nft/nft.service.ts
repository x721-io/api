import { sellStatus } from 'src/constants/enums/SellStatus.enum';
import { response } from 'express';
import { Erc1155Balance } from './../../generated/graphql';
import { CreateNftDto } from './dto/create-nft.dto';
import {
  Prisma,
  TX_STATUS,
  User,
  CONTRACT_TYPE,
  SELL_STATUS,
} from '@prisma/client';
import { PrismaService } from 'src/prisma/prisma.service';
import { NftDto } from './dto/nft.dto';
import {
  Injectable,
  HttpException,
  HttpStatus,
  NotFoundException,
  BadRequestException,
} from '@nestjs/common';
import { validate as isValidUUID } from 'uuid';
import { Redis } from 'src/database';
import { GetAllNftDto } from './dto/get-all-nft.dto';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';
import { MarketplaceService } from './nft-marketplace.service';
import { SellStatus, OrderDirection } from 'src/generated/graphql';
import { ZERO_ADDR } from 'src/constants/web3Const/messages';
import { OwnerOutputDto } from '../user/dto/owners.dto';
import { ValidatorService } from '../validator/validator.service';
import { GraphQLClient } from 'graphql-request';
import { GetActivityBase } from './dto/activity-nft.dto';
import { ActivityService } from './activity.service';
import { NftEntity } from './entities/nft.entity';
import { CollectionPriceService } from '../collection/collectionPrice.service';
import OtherCommon from 'src/commons/Other.common';
import subgraphServiceCommon from './helper/subgraph-helper.service';
import {
  creatorSelect,
  CollectionSelect,
  marketplaceSelect,
  nftSelect,
  userSelect,
  nftOwnerShip,
} from '../../commons/definitions/Constraint.Object';
import { GetGeneralInforDto } from './dto/get-general-infor.dto';
import { GeneralInfor } from 'src/constants/enums/GeneralInfor.enum';
import PaginationCommon from 'src/commons/HasNext.common';
import { NFTHepler } from './helper/nft-helper.service';
import { CreationMode } from 'src/constants/enums/Creation.enum';
import { ethers } from 'ethers';
import { UserService } from '../user/user.service';

@Injectable()
export class NftService {
  constructor(
    private prisma: PrismaService,
    private readonly GraphqlService: GraphQlcallerService,
    private readonly eventService: MarketplaceService,
    private validatorService: ValidatorService,
    private activityService: ActivityService,
    private collectionPriceService: CollectionPriceService,
    private nftHepler: NFTHepler,
    private userService: UserService,
  ) {}

  private readonly endpoint = process.env.SUBGRAPH_URL;
  private client = this.getGraphqlClient();
  private getGraphqlClient() {
    return new GraphQLClient(this.endpoint);
  }
  async crawlNftInfo(collectionAddress: string, txCreation?: string) {
    try {
      const collection = await this.prisma.collection.findUnique({
        where: { address: collectionAddress.toLowerCase() },
      });
      if (!collection) {
        throw new NotFoundException('Collection not found');
      }
      if (!txCreation) {
        await Redis.publish('nft-channel', {
          data: {
            type: collection.type,
            collectionAddress: collection.address,
          },
          process: 'nft-crawl-collection',
        });
        return true;
      } else {
        await Redis.publish('nft-channel', {
          data: {
            type: collection.type,
            txCreation: txCreation,
          },
          process: 'nft-crawl-single',
        });
      }
    } catch (err) {
      throw new Error(err);
    }
  }
  async create(input: CreateNftDto, user: User): Promise<NftDto> {
    try {
      let userCreator = user;
      const checkExist = await this.prisma.nFT.findFirst({
        where: {
          txCreationHash: input.txCreationHash,
        },
      });
      if (checkExist) {
        throw new BadRequestException('Transaction already submitted');
      }
      const collection = await this.prisma.collection.findFirst({
        where: {
          address: {
            mode: 'insensitive',
            contains: input.collectionId,
          },
        },
      });
      // if (!isValidUUID(input.creatorId)) {
      //   throw new Error('Invalid Creator ID. Please try again !');
      // }

      if (input.modeCreate == CreationMode.outside) {
        if (!input.creatorAddress) {
          throw new Error('Please enter creator address.');
        }
        if (!ethers.isAddress(input.creatorAddress)) {
          throw new Error('Invalid wallet address.');
        }
        userCreator = await this.userService.fetchOrCreateUser(
          input.creatorAddress,
        );
      }

      if (!collection) throw new NotFoundException('Collection not found');

      const collectionHasNameNFT =
        await this.validatorService.checkNFTExistence(
          'name',
          'collectionId',
          input.name,
          collection.id,
        );

      if (collectionHasNameNFT) {
        throw new Error('The name of the NFT already exists in Collection');
      }

      if (checkExist) {
        throw new Error('Transaction hash or ID already exists');
      }

      const nft = await this.prisma.nFT.create({
        data: {
          u2uId: input.u2uId,
          id: input.id,
          name: input.name,
          image: input.image,
          status: TX_STATUS.PENDING,
          tokenUri: input.tokenUri,
          txCreationHash: input.txCreationHash,
          creatorId: userCreator.id,
          collectionId: collection.id,
          animationUrl: input.animationUrl,
          source: input.source,
        },
        include: {
          traits: true,
          collection: true,
        },
      });
      await this.prisma.userNFT.create({
        data: {
          userId: userCreator.id,
          nftId: input.id,
          collectionId: collection.id,
        },
      });
      await Redis.publish('nft-channel', {
        data: {
          txCreation: nft.txCreationHash,
          type: nft.collection.type,
          collectionId: collection.id,
        },
        process: 'nft-create',
      });
      await Redis.publish('ipfs', {
        data: {
          collectionAddress: collection.address,
          tokenId: nft.id,
          ipfsUrl: nft.tokenUri.replace('ipfs://', ''),
        },
        process: 'get-ipfs',
      });
      return nft;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async findAll(filter: GetAllNftDto): Promise<PagingResponseHasNext<NftDto>> {
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
        const {
          nftIdFromOwner: ids = [],
          nftCollectionFromOwner: collections = [],
          hasNextNftOwner: hasNext = false,
        } = (await this.nftHepler.handleGetOwnerNFT(filter)) || {};
        nftIdFromOwner = ids;
        nftCollectionFromOwner = collections;
        hasNextNftOwner = hasNext;
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
      if (filter.source) {
        whereConditionInternal.AND.push({
          source: filter.source,
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
      } else {
        whereCondition.AND = whereConditionInternal.AND;
        delete whereCondition.OR;
      }
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
          const hasNext =
            (await PaginationCommon.hasNextPage(
              filter.page,
              filter.limit,
              'nFT',
              whereCondition1,
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
      }
    } catch (error) {
      console.error(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getNftDetailTransactionInfo(
    nftId: string,
    collectionAddress: string,
    page,
    limit,
  ) {
    const collection = await this.prisma.collection.findUnique({
      where: {
        address: collectionAddress.toLowerCase(),
      },
    });
    if (!collection) {
      throw new NotFoundException('No collection was found');
    }
    const nft: NftEntity = await this.prisma.nFT.findUnique({
      where: {
        id_collectionId: {
          id: nftId,
          collectionId: collection.id,
        },
      },
      include: {
        creator: {
          select: creatorSelect,
        },
        collection: {
          include: {
            category: {
              select: {
                id: true,
                name: true,
              },
            },
          },
        },
        traits: true,
      },
    });
    if (!nft) {
      throw new NotFoundException();
    }
    let owners: any, totalSupply: any;
    if (collection.flagExtend == true) {
      ({ owners, totalSupply } = await this.getCurrentOwnersExtend(
        collection.subgraphUrl,
        nft,
      ));
    } else {
      ({ owners, totalSupply } = await this.getCurrentOwnersInternal(nft));
    }
    const sellInfo = await this.eventService.findEvents({
      contractAddress: nft.collection.address,
      nftId: nft.u2uId ? nft.u2uId : nft.id,
      event: SellStatus.AskNew,
      type: nft.collection.type,
      page: 0,
      limit: owners.length > 0 ? owners.length : 1,
    });

    const bidInfo = await this.eventService.findEvents({
      contractAddress: nft.collection.address,
      nftId: nft.u2uId ? nft.u2uId : nft.id,
      event: SellStatus.Bid,
      type: nft.collection.type,
      page: (page - 1) * limit,
      limit: limit,
    });

    const bidderAddress = bidInfo.map((bidder) => bidder.to);

    const bidderInfo = await this.prisma.user.findMany({
      where: {
        signer: {
          in: bidderAddress,
        },
      },
      select: userSelect,
    });

    const sellerAddress = bidInfo.concat(sellInfo).map((seller) => seller.from);

    const sellerInfo = await this.prisma.user.findMany({
      where: {
        signer: {
          in: sellerAddress.filter((i) => i !== null),
        },
      },
      select: userSelect,
    });

    const mergedBidder = bidInfo.map((item) => {
      const match = bidderInfo.find((item1) => item1.signer == item.to);
      return match ? { ...item, to: match as OwnerOutputDto } : item;
    });

    const mergedSeller = sellInfo.map((item) => {
      const match = sellerInfo.find((item1) => item1.signer == item.from);
      return match ? { ...item, from: match as OwnerOutputDto } : item;
    });
    return {
      bidInfo: mergedBidder,
      sellInfo: mergedSeller,
      owners,
      totalSupply,
    };
  }

  async getCurrentOwnersExtend(
    subgraphUri: string,
    nft: NftEntity,
  ): Promise<{ owners: OwnerOutputDto[]; totalSupply: number }> {
    let owners: OwnerOutputDto[] = [];
    let nftInfoWithOwner;
    let totalSupply = 0;
    if (nft.collection.type === 'ERC1155') {
      nftInfoWithOwner = await subgraphServiceCommon.subgraphQuery(
        subgraphUri,
        CONTRACT_TYPE.ERC1155,
        nft.id,
      );
      const totalSupplyFilter = nftInfoWithOwner?.userBalances?.filter(
        (i) => i.balance > 0 && i?.token?.balance > 0,
      );

      totalSupply =
        totalSupplyFilter?.length > 0 &&
        totalSupplyFilter?.[0] &&
        totalSupplyFilter[0]?.token?.balance;
      const ownerAddresses = nftInfoWithOwner?.userBalances
        ?.map((i) => {
          if (i.owner && i.owner.id !== ZERO_ADDR && i?.token?.balance > 0) {
            return i.owner.id;
          }
        })
        .filter((i) => !!i);
      const ownersFromLocal = await this.prisma.user.findMany({
        where: {
          signer: { in: ownerAddresses },
        },
        select: creatorSelect,
      });
      owners = ownersFromLocal.map((item2) => {
        const item1 = nftInfoWithOwner.userBalances.find(
          (i1) => i1.owner && i1.owner.id === item2.signer,
        );
        if (item1) {
          return {
            ...item2,
            publicKey: item2?.publicKey ?? item2?.signer,
            username: item2?.username ?? item2?.signer,
            quantity: item1?.balance,
          };
        }
        return item2;
      });
    } else {
      nftInfoWithOwner = await subgraphServiceCommon.subgraphQuery(
        subgraphUri,
        CONTRACT_TYPE.ERC721,
        nft.id,
      );
      totalSupply = 1;
      const ownerId = nftInfoWithOwner?.items?.[0]?.owner?.id;
      owners = await this.prisma.user.findMany({
        where: {
          signer: ownerId,
        },
        select: creatorSelect,
      });
    }

    if (owners.length === 0) {
      if (nft.collection.type === 'ERC1155') {
        const ownersNon =
          nftInfoWithOwner?.userBalances?.map((item) => ({
            signer: item?.owner?.id || '',
            quantity: item?.balance || 0,
          })) || [];

        return {
          owners: ownersNon,
          totalSupply: totalSupply,
        };
      } else {
        return {
          owners: [
            {
              signer: nftInfoWithOwner?.items?.[0]?.owner?.id || '',
            },
          ],
          totalSupply,
        };
      }
    } else {
      return { owners, totalSupply };
    }
  }

  async getCurrentOwnersInternal(
    nft: NftEntity,
  ): Promise<{ owners: OwnerOutputDto[]; totalSupply: number }> {
    let owners: OwnerOutputDto[] = [];
    let nftInfoWithOwner;
    let totalSupply = 0;
    if (nft.collection.type === 'ERC1155') {
      nftInfoWithOwner = await this.GraphqlService.getOneNFTOwnersInfo1155(
        nft.collection.address,
        nft.u2uId ? nft.u2uId : nft.id,
      );
      const totalSupplyFilter = nftInfoWithOwner.erc1155Balances.filter(
        (i) => i.value > 0 && !i.account,
      );
      totalSupply = totalSupplyFilter[0].value;
      const ownerAddresses = nftInfoWithOwner.erc1155Balances
        .map((i) => {
          if (i.account && i.account.id !== ZERO_ADDR && i.value > 0)
            return i.account.id;
        })
        .filter((i) => !!i);
      const ownersFromLocal = await this.prisma.user.findMany({
        where: {
          signer: { in: ownerAddresses },
        },
        select: creatorSelect,
      });
      owners = ownersFromLocal.map((item2) => {
        const item1 = nftInfoWithOwner.erc1155Balances.find(
          (i1) => i1.account && i1.account.id === item2.signer,
        );
        if (item1) {
          return {
            ...item2,
            quantity: item1.value,
          };
        }
        return item2;
      });
    } else {
      nftInfoWithOwner = await this.GraphqlService.getOneNFTOwnersInfo721(
        nft.collection.address,
        nft.u2uId ? nft.u2uId : nft.id,
      );
      totalSupply = 1;
      owners = await this.prisma.user.findMany({
        where: {
          signer: nftInfoWithOwner.erc721Tokens[0].owner.id,
        },
        select: creatorSelect,
      });
    }
    if (owners.length === 0) {
      return {
        // @ts-ignore
        owners: [{ signer: nftInfoWithOwner.erc721Tokens[0].owner.id }],
        totalSupply,
      };
    } else {
      return { owners, totalSupply };
    }
  }

  async findOne(id: string, collectionAddress: string): Promise<NftDto> {
    try {
      const collection = await this.prisma.collection.findUnique({
        where: {
          address: collectionAddress.toLowerCase(),
        },
      });

      if (!collection) {
        throw new NotFoundException('No collection was found');
      }

      const nftCondition: Prisma.NFTWhereInput = {};
      nftCondition.OR = [];

      const nftOrConditionId: Prisma.NFTWhereInput = {
        AND: [{ id, collectionId: collection.id }],
      };
      const nftOrConditionu2uId: Prisma.NFTWhereInput = {
        AND: [{ u2uId: id, collectionId: collection.id }],
      };

      nftCondition.OR.push(nftOrConditionId, nftOrConditionu2uId);

      const nft: NftEntity = await this.prisma.nFT.findFirst({
        where: nftCondition,
        include: {
          creator: {
            select: creatorSelect,
          },
          collection: {
            include: {
              category: {
                select: {
                  id: true,
                  name: true,
                },
              },
            },
          },
          traits: true,
        },
      });
      if (!nft) {
        throw new NotFoundException('No NFT found');
      }
      let owners: OwnerOutputDto[];
      if (nft.collection.type === 'ERC1155') {
        // const ownerAndSupplyInfo = await this.getCurrentOwners(nft);
        // owners = ownerAndSupplyInfo.owners;
        // nft.totalSupply = ownerAndSupplyInfo.totalSupply;
      } else {
        // const ownerAndSupplyInfo = await this.getCurrentOwners(nft);
        // owners = ownerAndSupplyInfo.owners;
      }
      // @ts-ignore
      nft.owners = owners;
      // const sellInfo = await this.eventService.findEvents({
      //   contractAddress: nft.collection.address,
      //   nftId: nft.u2uId ? nft.u2uId : nft.id,
      //   event: SellStatus.AskNew,
      //   type: nft.collection.type,
      //   page: 0,
      //   limit: owners.length > 0 ? owners.length : 1,
      // });

      // const bidInfo = await this.eventService.findEvents({
      //   contractAddress: nft.collection.address,
      //   nftId: nft.u2uId ? nft.u2uId : nft.id,
      //   event: SellStatus.Bid,
      //   type: nft.collection.type,
      //   page: (bidPage - 1) * bidListLimit,
      //   limit: bidListLimit,
      // });
      const royalties =
        await this.collectionPriceService.FetchRoyaltiesFromGraph(
          collectionAddress,
        );
      const totalRoyalties = royalties.reduce(
        (acc, item) => acc + item.value,
        0,
      );
      const returnNft: NftDto = {
        ...nft,
        collection: {
          ...nft.collection,
          totalRoyalties,
          listRoyalties: royalties,
        },
        // sellInfo: sellInfo,
        // bidInfo: bidInfo,
      };
      return returnNft;
    } catch (error) {
      console.error(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async findNFTByUserID(id: string): Promise<any[]> {
    try {
      if (!isValidUUID(id)) {
        throw new Error('Invalid User. Please try again !');
      }
      const checkExist = await this.prisma.user.findFirst({
        where: { id: id },
      });
      if (!checkExist) {
        throw new NotFoundException();
      }
      return this.prisma.user.findMany({
        where: {
          id: id,
        },
        include: {
          nftsOwnership: {
            select: nftOwnerShip,
          },
        },
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async findActivityNFT(input: GetActivityBase) {
    try {
      const { tokenId, quoteToken, collectionAddress, page, limit, type } =
        input;

      const and = [{ tokenId }, { quoteToken }, { address: collectionAddress }];
      const blocks = await this.activityService.fetchActivityFromGraph({
        and,
        page,
        limit,
        type,
      });

      const result = await this.activityService.processActivityNFTData(blocks);
      return result;
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  getUserData = async (signer: string) => {
    try {
      return await this.prisma.user.findFirst({
        where: { signer },
        select: userSelect,
      });
    } catch (error) {
      console.error(`Error fetching user data for signer ${signer}:`, error);
      throw error; // You may want to handle or log the error accordingly
    }
  };

  async getGeneralInfor(filter: GetGeneralInforDto) {
    try {
      switch (filter.mode) {
        // // Get Owner NFT
        case GeneralInfor.OWNER:
          // const { nftIdFromOwner = [] } =
          //   await this.nftHepler.handleGetOwnerNFT({
          //     owner: filter.creatorAddress || filter.owner,
          //     page: 1,
          //     limit: 1000,
          //   });

          // return nftIdFromOwner?.length || 0;
          const { erc721Tokens = [], erc1155Balances = [] } =
            await this.GraphqlService.getNFTExternalFromOwner(
              filter.owner.toLowerCase(),
              'desc' as OrderDirection,
              1,
              1000,
            );
          const { account } = await this.GraphqlService.getNFTFromOwner(
            filter.owner.toLowerCase(),
            'desc' as OrderDirection,
            1,
            1000,
          );
          // const { ERC721tokens = [], ERC1155balances = [] } = account;
          let internal721Filter = [];
          let internal1155Filter = [];

          if (account) {
            internal721Filter = await this.nftHepler.filterExistingNFTs(
              account?.ERC721tokens,
              (item) => item?.tokenId,
              (item) => item?.contract?.id,
              false,
            );

            internal1155Filter = await this.nftHepler.filterExistingNFTs(
              account?.ERC1155balances,
              (item) => item?.token?.tokenId,
              (item) => item?.token?.contract?.id,
              false,
            );
          }

          const external721Filter = await this.nftHepler.filterExistingNFTs(
            erc721Tokens,
            (item) => item?.tokenID,
            (item) => item?.contract,
            true,
          );
          const external1155Filter = await this.nftHepler.filterExistingNFTs(
            erc1155Balances,
            (item) => item?.token?.tokenID,
            (item) => item?.token?.contract,
            true,
          );

          const countERC1155 = this.nftHepler.reduceData1155([
            ...internal1155Filter,
            ...external1155Filter,
          ]);
          const countERC721 = this.nftHepler.reduceData721([
            ...internal721Filter,
            ...external721Filter,
          ]);
          const countHolding =
            (countERC721.length || 0) + (countERC1155.length || 0);

          return countHolding;
        case GeneralInfor.CREATOR:
          const totalOwnerCreator = await this.prisma.userNFT.count({
            where: {
              user: {
                signer: filter.creatorAddress.toLowerCase(),
              },
              nft: {
                status: TX_STATUS.SUCCESS,
              },
            },
          });
          return totalOwnerCreator;
        case GeneralInfor.ONSALES:
          // ======= 1 : Get from Marketplace Status
          // const listSales = await this.prisma.marketplaceStatus.findMany({
          //   where: {
          //     event: SellStatus.AskNew,
          //     from: filter.owner.toLowerCase(),
          //   },
          // });
          // const sales = await this.nftHepler.filterExistingNFTs(
          //   listSales,
          //   (item) => item?.tokenId,
          //   (item) => item?.collectionId,
          // );
          // return sales?.length || 0;
          // =======2 : Get From On Sales in Subgraph
          // const response = await this.GraphqlService.getNFTOnSalesAndOwner(
          //   filter.owner.toLowerCase(),
          // );
          // return (response && response.onSaleCount) || 0;
          // =======3 : Get From Subgraph
          const { marketEvent1155S = [], marketEvent721S = [] } =
            await this.GraphqlService.getNFTOnSales(filter.owner.toLowerCase());
          const marketEvent1155SFilter =
            await this.nftHepler.filterExistingNFTs(
              marketEvent1155S,
              (item) => item?.nftId?.tokenId,
              (item) => item?.nftId?.contract?.id,
            );

          const marketEvent721SFilter = await this.nftHepler.filterExistingNFTs(
            marketEvent721S,
            (item) => item?.nftId?.tokenId,
            (item) => item?.nftId?.contract?.id,
          );
          const countOnSalse =
            [...marketEvent721SFilter, ...marketEvent1155SFilter].length || 0;
          return countOnSalse;
        case GeneralInfor.COLLECTION:
          let isUuid = true;
          if (!isValidUUID(filter.owner)) {
            isUuid = false;
          }
          const totalCollection = await this.prisma.userCollection.count({
            where: {
              user: {
                ...(isUuid
                  ? { id: filter.owner }
                  : {
                      OR: [{ signer: filter.owner.toLowerCase() }],
                    }),
              },
              collection: {
                status: TX_STATUS.SUCCESS,
              },
            },
          });
          return totalCollection;
      }
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
}
