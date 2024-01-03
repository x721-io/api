import {
  Injectable,
  HttpException,
  HttpStatus,
  NotFoundException,
} from '@nestjs/common';
import { CreateCollectionDto } from './dto/create-collection.dto';
import { UpdateCollectionDto } from './dto/update-collection.dto';
import { CollectionEntity } from './entities/collection.entity';
import { PrismaService } from 'src/prisma/prisma.service';
import { CONTRACT_TYPE, Prisma, TX_STATUS, User } from '@prisma/client';
import { validate as isValidUUID } from 'uuid';
import { Redis } from 'src/database';
import { TraitGeneralInfo, TraitService } from '../nft/trait.service';
import { GetAllCollectionDto } from './dto/get-all-collection.dto';
import { GetCollectionMarketData } from '../graph-qlcaller/getCollectionMarketData.service';
import { CollectionPriceService } from './collectionPrice.service';
import { GetCollectionByUserDto } from './dto/get-collection-by-user.dto';
import SecureUtil from '../../commons/Secure.common';
import { GraphQLClient } from 'graphql-request';
import { getSdk } from '../../generated/graphql';
import { oneWeekInMilliseconds } from '../../constants/Timestamp.constant';
interface CollectionGeneral {
  totalOwner: number;
  volumn: string;
  totalNft: number;
  floorPrice: string;
}

interface CollectionVolumeInterface {
  timestamp: string;
  total: string;
}

@Injectable()
export class CollectionService {
  constructor(
    private prisma: PrismaService,
    private traitService: TraitService,
    private readonly collectionData: GetCollectionMarketData,
    private readonly collectionPriceService: CollectionPriceService,
  ) {}

  private readonly endpoint = process.env.SUBGRAPH_URL;
  private client = this.getGraphqlClient();
  private getGraphqlClient() {
    return new GraphQLClient(this.endpoint);
  }

  private sdk = getSdk(this.client);

  async create(input: CreateCollectionDto, user: User): Promise<any> {
    try {
      const checkExist = await this.prisma.collection.findFirst({
        where: {
          OR: [
            { txCreationHash: input.txCreationHash },
            { symbol: input.symbol },
            { name: input.name },
            { shortUrl: input.shortUrl },
          ],
        },
      });
      if (checkExist) {
        throw new Error(
          'Transaction hash or name or Short URL , symbol are already exists',
        );
      } else {
        const collection = await this.prisma.collection.create({
          data: {
            txCreationHash: input.txCreationHash,
            name: input.name,
            symbol: input.symbol,
            description: input.description,
            status: TX_STATUS.PENDING,
            type: input.type,
            shortUrl: input.shortUrl,
            coverImage: input.coverImage,
            metadata: input.metadata,
            avatar: input.avatar,
            // categoryId: ...(input.categoryId  Number(input.categoryId),
            ...(input.categoryId && { categoryId: Number(input.categoryId) }),
          },
        });

        await this.prisma.userCollection.create({
          data: {
            userId: user.id,
            collectionId: collection.id,
          },
        });
        await Redis.publish('collection-channel', {
          data: {
            txCreation: collection.txCreationHash,
            type: collection.type,
          },
          process: 'collection-create',
        });
        return collection;
      }
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getGeneralCollectionData(
    collectionAddress: string,
    type: CONTRACT_TYPE,
  ): Promise<CollectionGeneral> {
    const [response, count, sum] = await Promise.all([
      this.collectionData.getCollectionSumData(collectionAddress),
      this.collectionData.getCollectionTokens(collectionAddress),
      this.getVolumeCollection(collectionAddress),
    ]);
    if (type === 'ERC721') {
      const uniqueOwnerIdsCount = new Set(
        count.erc721Tokens.map((obj) => obj.owner.id),
      ).size;
      const filterSelling = response.marketEvent721S.filter(
        (obj) => obj.event === 'AskNew',
      );
      const floorPrice =
        filterSelling.length > 0
          ? filterSelling.reduce(
              (min, obj) =>
                BigInt(obj.price) < BigInt(min)
                  ? BigInt(obj.price)
                  : BigInt(min),
              BigInt(filterSelling[0].price),
            )
          : BigInt(0);
      return {
        volumn: sum.toString(),
        totalOwner: uniqueOwnerIdsCount,
        totalNft: count.erc721Tokens.length,
        floorPrice: floorPrice.toString(),
      };
    } else {
      // const respose = await this.collectionData.getCollectionSumData('0xc2587c1b945b1a7be4be5423c24f1bbf54495daa')
      // count owners
      const owners =
        await this.collectionData.getCollectionHolder(collectionAddress);
      const uniqueOwnerIdsCount = owners.erc1155Balances.filter(
        (obj) => BigInt(obj.value) > BigInt(0) && !!obj.account,
      ).length;
      // volumn

      // filter floor price
      const filterSelling = response.marketEvent1155S.filter(
        (obj) => obj.event === 'AskNew',
      );
      const floorPrice =
        filterSelling.length > 0
          ? filterSelling.reduce(
              (min, obj) =>
                BigInt(obj.price) < BigInt(min)
                  ? BigInt(obj.price)
                  : BigInt(min),
              BigInt(filterSelling[0].price),
            )
          : BigInt(0);
      // filter name
      return {
        volumn: sum.toString(),
        totalOwner: uniqueOwnerIdsCount,
        totalNft: count.erc1155Tokens.length,
        floorPrice: floorPrice.toString(),
      };
    }
  }

  async findAll(
    input: GetAllCollectionDto,
  ): Promise<PagingResponse<CollectionEntity>> {
    // TODO: get all collection from subgraph first, got the id and map it back to local collection
    const creators = await this.prisma.user.findMany({
      where: {
        publicKey: {
          in: input.creatorAddresses,
          mode: 'insensitive',
        },
      },
    });
    const addresses = creators.map((item) => item.id);
    let whereCondition: Prisma.CollectionWhereInput = {
      ...(input.name && {
        name: {
          contains: input.name,
          mode: 'insensitive',
        },
      }),
      creators: {
        some: {
          userId: {
            in: addresses,
          },
        },
      },
      status: TX_STATUS.SUCCESS,
    };
    let addressObject = {};
    if (input.max || input.min) {
      const min = Number(input.min);
      const max = Number(input.max);
      if (min > max) {
        return {
          data: [],
          paging: {
            total: 0,
            page: input.page,
            limit: input.limit,
          },
        };
      }
      // IF min == 0 and max > 0 => | Get all item from item don't have value in subgraph and value < max
      if (min === 0) {
        const filteredContractId =
          await this.collectionPriceService.filterFloorPriceFromSubgraphWithoutMinMax();
        addressObject = {
          address: {
            notIn: filteredContractId,
          },
        };
      }
      const filteredContractId =
        await this.collectionPriceService.filterFloorPriceFromSubgraph(
          input.min,
          input.max,
        );
      addressObject = {
        OR: [{ ...addressObject }, { address: { in: filteredContractId } }],
      };
      whereCondition = {
        ...whereCondition,
        ...addressObject,
      };
      const filterPriceCollection = await this.prisma.collection.findMany({
        where: whereCondition,
        skip: (input.page - 1) * input.limit,
        take: input.limit,
        include: {
          creators: {
            select: {
              userId: true,
              user: {
                select: {
                  id: true,
                  email: true,
                  avatar: true,
                  username: true,
                  publicKey: true,
                },
              },
            },
          },
        },
      });
      const subgraphCollection = filterPriceCollection.map(async (item) => {
        const generalInfo = await this.getGeneralCollectionData(
          item.address,
          item.type,
        );
        return { ...item, ...generalInfo };
      });
      const dataArray = Promise.all(subgraphCollection);

      const total = await this.prisma.collection.count({
        where: whereCondition,
      });
      return {
        data: await dataArray,
        paging: {
          total,
          page: input.page,
          limit: input.limit,
        },
      };
    } else {
      // IF Min and Max == 0 => | return just item don't have value in subgraph
      // ÌF Just Max == 0    => | return just item don't have value in subgraph
      if (
        (Number(input.min) == 0 && Number(input.max) == 0) ||
        Number(input.max) == 0
      ) {
        const filteredContractId =
          await this.collectionPriceService.filterFloorPriceFromSubgraphWithoutMinMax();
        whereCondition.address = {
          notIn: filteredContractId,
        };
      }
      const collections = await this.prisma.collection.findMany({
        where: whereCondition,
        skip: (input.page - 1) * input.limit,
        take: input.limit,
        include: {
          creators: {
            select: {
              userId: true,
              user: {
                select: {
                  id: true,
                  email: true,
                  avatar: true,
                  username: true,
                  publicKey: true,
                },
              },
            },
          },
        },
      });
      const total = await this.prisma.collection.count({
        where: whereCondition,
      });
      const subgraphCollection = collections.map(async (item) => {
        const generalInfo = await this.getGeneralCollectionData(
          item.address,
          item.type,
        );
        return { ...item, ...generalInfo };
      });
      const dataArray = Promise.all(subgraphCollection);
      return {
        data: await dataArray,
        paging: {
          total,
          limit: input.limit,
          page: input.page,
        },
      };
    }
  }

  async findOne(id: string): Promise<{
    collection: CollectionEntity;
    traitAvailable: TraitGeneralInfo[];
    generalInfo: any;
  }> {
    try {
      let whereCondition: Prisma.CollectionWhereInput;
      if (!isValidUUID(id)) {
        // throw new Error('Invalid ID. Please try again !');
        whereCondition = {
          shortUrl: id,
        };
      } else {
        whereCondition = {
          id,
        };
      }

      const collection = await this.prisma.collection.findFirst({
        where: whereCondition,
        include: {
          category: {
            select: {
              id: true,
              name: true,
            },
          },
          creators: {
            select: {
              userId: true,
              user: {
                select: {
                  id: true,
                  email: true,
                  avatar: true,
                  username: true,
                  publicKey: true,
                  createdAt: true,
                },
              },
            },
          },
        },
      });
      if (!collection) {
        throw new NotFoundException();
      }
      const traitsAvailable =
        await this.traitService.findUniqueTraitsInCollection(collection.id);
      const generalInfo = await this.getGeneralCollectionData(
        collection.address,
        collection.type,
      );
      return { collection, traitAvailable: traitsAvailable, generalInfo };
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async update(
    id: string,
    input: UpdateCollectionDto,
    user: User,
  ): Promise<any> {
    try {
      // Validate ID
      if (!isValidUUID(id)) {
        throw new Error('Invalid ID. Please try again!');
      }

      // Check if Collection Exists
      const existingCollection = await this.prisma.collection.findFirst({
        where: { id: id },
      });

      if (!existingCollection) {
        throw new NotFoundException();
      }

      // Check if User is the Creator of the Collection
      const isCreator = await this.prisma.userCollection.findFirst({
        where: { AND: [{ userId: user.id }, { collectionId: id }] },
      });

      if (!isCreator) {
        throw new Error(`You can't update this collection`);
      }

      // Update Collection
      const updatedCollection = await this.prisma.collection.update({
        where: { id: id },
        data: {
          description: input.description,
          coverImage: input.coverImage,
        },
      });

      return updatedCollection;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async findWithUserID(
    id: string,
    input: GetCollectionByUserDto,
  ): Promise<PagingResponse<CollectionEntity>> {
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
      const userWithCollection = await this.prisma.userCollection.findMany({
        where: {
          userId: id,
        },
        skip: (input.page - 1) * input.limit,
        take: input.limit,
        include: {
          collection: {
            select: {
              id: true,
              txCreationHash: true,
              name: true,
              address: true,
              metadata: true,
              shortUrl: true,
              symbol: true,
              description: true,
              status: true,
              type: true,
              categoryId: true,
              createdAt: true,
              avatar: true,
              coverImage: true,
              updatedAt: true,
              projectId: true,
              isU2U: true,
              category: {
                select: {
                  id: true,
                  name: true,
                },
              },
            },
          },
        },
      });
      const response = userWithCollection.map((item) => {
        return { collection: item.collection };
      });

      if (input.hasBase) {
        const baseCollection721 = await this.prisma.collection.findUnique({
          where: {
            address: process.env.BASE_ADDR_721,
          },
          include: {
            category: true,
          },
        });
        const baseCollection1155 = await this.prisma.collection.findUnique({
          where: {
            address: process.env.BASE_ADDR_1155,
          },
          include: {
            category: true,
          },
        });

        response.push({ collection: baseCollection721 });
        response.push({ collection: baseCollection1155 });
      }

      const total = await this.prisma.userCollection.count({
        where: {
          userId: id,
        },
      });

      return {
        data: response.map((i) => i.collection),
        paging: {
          total: total,
          limit: input.limit,
          page: input.page,
        },
      };
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
  async checkRecord(address: string) {
    try {
      const result = await SecureUtil.getSessionInfo(address);
      return result ? JSON.parse(result) : null;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async saveVolumeCollection(
    address: string,
    input: CollectionVolumeInterface,
  ) {
    try {
      const result = await SecureUtil.storeObjectSession(
        address,
        input,
        oneWeekInMilliseconds,
      );
      return result;
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getVolumeCollection(address: string) {
    const { blocks = [] } = await this.sdk.getActivity({ address });
    const redisData = await this.checkRecord(address);
    const lastUpdate = `${blocks?.[0]?.timestampt || ''}`;
    const isTradeOrAcceptBid = (item: any) =>
      item.event === 'Trade' || item.event === 'AcceptBid';
    const sum = blocks
      .filter(isTradeOrAcceptBid)
      .reduce((acc, obj) => acc + BigInt(obj.price), BigInt(0));

    if (redisData !== null) {
      const redisTimestamp = parseInt(redisData.timestamp, 10);

      const newBlocks = blocks.filter(
        (item) => item.timestampt > redisTimestamp,
      );

      if (newBlocks.length > 0) {
        const sumNewBlock = newBlocks
          .filter(isTradeOrAcceptBid)
          .reduce((acc, obj) => acc + BigInt(obj.price), BigInt(0));
        const updatedTotal = (BigInt(redisData.total) + sumNewBlock).toString();
        await this.saveVolumeCollection(address, {
          timestamp: lastUpdate,
          total: updatedTotal,
        });
        return updatedTotal;
      }

      return sum;
    } else {
      await this.saveVolumeCollection(address, {
        timestamp: lastUpdate,
        total: sum.toString(),
      });
      return sum;
    }
  }
}
