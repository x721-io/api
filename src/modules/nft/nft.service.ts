import { CreateNftDto } from './dto/create-nft.dto';
import { UpdateNftDto } from './dto/update-nft.dto';
import { Prisma, TX_STATUS, User } from '@prisma/client';
import { PrismaService } from 'src/prisma/prisma.service';
import { NftDto } from './dto/nft.dto';
import {
  Injectable,
  HttpException,
  HttpStatus,
  NotFoundException,
} from '@nestjs/common';
import { validate as isValidUUID } from 'uuid';
import { Redis } from 'src/database';
import { GetAllNftDto } from './dto/get-all-nft.dto';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';
import { MarketplaceService } from './nft-marketplace.service';
import { SellStatus } from 'src/generated/graphql';
import { ZERO_ADDR } from 'src/constants/web3Const/messages';
import { OwnerOutputDto } from '../user/dto/owners.dto';
import { ValidatorService } from '../validator/validator.service';

@Injectable()
export class NftService {
  constructor(
    private prisma: PrismaService,
    private readonly GraphqlService: GraphQlcallerService,
    private readonly eventService: MarketplaceService,
    private validatorService: ValidatorService,
  ) {}
  async create(input: CreateNftDto, user: User): Promise<NftDto> {
    try {
      const checkExist = await this.prisma.nFT.findFirst({
        where: {
          OR: [{ txCreationHash: input.txCreationHash }, { id: input.id }],
        },
      });

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

      if (!collection) throw new NotFoundException('Collection not found');

      // if (!isValidUUID(input.collectionId)) {
      //   throw new Error('Invalid Collection ID. Please try again !');
      // }
      const collectionHasNameNFT =
        await this.validatorService.checkNFTExistence(
          'name',
          'collectionId',
          input.name,
          input.collectionId,
        );

      if (collectionHasNameNFT) {
        throw new Error('The name of the NFT already exists in Collection');
      }

      if (checkExist) {
        throw new Error('Transaction hash or ID already exists');
      }

      const nft = await this.prisma.nFT.create({
        data: {
          id: input.id,
          name: input.name,
          ipfsHash: input.name,
          imageHash: input.imageHash,
          traits: {
            create: input.traits,
          },
          status: TX_STATUS.PENDING,
          tokenUri: input.tokenUri,
          txCreationHash: input.txCreationHash,
          creatorId: user.id,
          collectionId: collection.id,
        },
        include: {
          traits: true,
          collection: true,
        },
      });
      await this.prisma.userNFT.create({
        data: {
          userId: user.id,
          nftId: input.id,
        },
      });
      await Redis.publish(
        'nft-channel',
        JSON.stringify({
          txCreation: nft.txCreationHash,
          type: nft.collection.type,
        }),
      );
      return nft;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async findAll(filter: GetAllNftDto): Promise<PagingResponse<NftDto>> {
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
      if (filter.owner) {
        const { account } = await this.GraphqlService.getNFTFromOwner(
          filter.owner.toLocaleLowerCase(),
        );
        nftIdFromOwner = account.ERC721tokens.map((item) => item.id).concat(
          account.ERC1155balances.map((item) => item.id.split('-')[1]),
        );
      }
      let whereCondition: Prisma.NFTWhereInput = {
        AND: traitsConditions,
        ...(filter.creatorAddress && {
          creator: {
            publicKey: filter.creatorAddress,
          },
        }),
        collection: {
          ...(filter.collectionAddress && {
            address: filter.collectionAddress,
          }),
          ...(filter.type && { type: filter.type }),
        },
        ...(filter.name && { name: filter.name }),
        ...(nftIdFromOwner.length > 0 && {
          id: {
            in: nftIdFromOwner,
          },
        }),
      };
      const { marketEvent1155S, marketEvent721S } =
        await this.GraphqlService.getNFTSellStatus1(
          {
            and: [
              { price_gte: filter.priceMin },
              { price_lte: filter.priceMax },
              { event: filter.sellStatus },
              { quoteToken: filter.quoteToken },
              { from: filter.from },
              { to: filter.to },
            ],
            // or: [{ from: filter.owner }, { to: filter.owner }],
          },
          // 1,
          // 1,
        );
      if (!filter.priceMin && !filter.priceMax && !filter.sellStatus) {
        const nfts = await this.prisma.nFT.findMany({
          skip: (filter.page - 1) * filter.limit,
          take: filter.limit,
          where: whereCondition,
          include: {
            creator: {
              select: {
                id: true,
                email: true,
                avatar: true,
                username: true,
                publicKey: true,
              },
            },
            collection: {
              select: {
                id: true,
                txCreationHash: true,
                name: true,
                status: true,
                type: true,
                address: true,
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
        const mergedArray = nfts.map((item) => {
          const foundItem1 = marketEvent721S.find(
            (obj) => obj.nftId.id === item.id,
          );
          const foundItem2 = marketEvent1155S.find(
            (obj) => obj.nftId.id === item.id,
          );
          return {
            ...item,
            ...(foundItem1 && {
              price: foundItem1.price,
              sellStatus: foundItem1.event,
              quantity: 1,
              quoteToken: foundItem1.quoteToken,
            }),
            ...(foundItem2 && {
              price: foundItem2.price,
              sellStatus: foundItem2.event,
              quantity: foundItem2.amounts,
              askId: foundItem2.id,
              quoteToken: foundItem2.quoteToken,
            }),
          };
        });
        const total = await this.prisma.nFT.count({
          where: whereCondition,
        });
        return {
          data: mergedArray,
          paging: {
            total,
            limit: filter.limit,
            page: filter.page,
          },
        };
      } else {
        whereCondition = {
          ...whereCondition,
          id: {
            in: marketEvent721S
              .map((item) => item.nftId.id)
              .concat(marketEvent1155S.map((item) => item.nftId.id)),
          },
        };
        const nfts = await this.prisma.nFT.findMany({
          skip: (filter.page - 1) * filter.limit,
          take: filter.limit,
          where: whereCondition,
          include: {
            creator: {
              select: {
                id: true,
                email: true,
                avatar: true,
                username: true,
                publicKey: true,
              },
            },
            collection: {
              select: {
                id: true,
                txCreationHash: true,
                name: true,
                status: true,
                type: true,
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

        const mergedArray = nfts.map((item) => {
          const foundItem1 = marketEvent721S.find(
            (obj) => obj.nftId.id === item.id,
          );
          const foundItem2 = marketEvent1155S.find(
            (obj) => obj.nftId.id === item.id,
          );
          return {
            ...item,
            ...(foundItem1 && {
              price: foundItem1.price,
              sellStatus: foundItem1.event,
              quantity: 1,
              quoteToken: foundItem1.quoteToken,
            }),
            ...(foundItem2 && {
              price: foundItem2.price,
              sellStatus: foundItem2.event,
              quantity: foundItem2.amounts,
              askId: foundItem2.id,
              quoteToken: foundItem2.quoteToken,
            }),
          };
        });
        const total = await this.prisma.nFT.count({
          where: whereCondition,
        });
        return {
          data: mergedArray,
          paging: {
            total,
            limit: filter.limit,
            page: filter.page,
          },
        };
      }
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async findOne(
    id: string,
    bidPage: number,
    bidListLimit: number,
  ): Promise<NftDto> {
    try {
      const nft = await this.prisma.nFT.findUnique({
        where: {
          id: id,
        },
        include: {
          creator: {
            select: {
              id: true,
              email: true,
              avatar: true,
              username: true,
              publicKey: true,
            },
          },
          collection: {
            select: {
              id: true,
              txCreationHash: true,
              name: true,
              status: true,
              type: true,
              address: true,
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
      let owners: OwnerOutputDto[] = [];
      if (nft.collection.type === 'ERC1155') {
        const nftInfoWithOwner =
          await this.GraphqlService.getOneNFTOwnersInfo1155(id);
        const ownerAddresses = nftInfoWithOwner.erc1155Balances
          .map((i) => {
            if (i.account && i.account.id !== ZERO_ADDR) return i.account.id;
          })
          .filter((i) => !!i);
        const ownersFromLocal = await this.prisma.user.findMany({
          where: {
            signer: { in: ownerAddresses },
          },
          select: {
            email: true,
            avatar: true,
            username: true,
            signer: true,
            publicKey: true,
          },
        });
        owners = ownersFromLocal.map((item2) => {
          const item1 = nftInfoWithOwner.erc1155Balances.find(
            (i1) => i1.account.id === item2.signer,
          );
          if (item1) {
            return {
              ...item2,
              quantity: item1.value, // Add other fields from item1 as needed
            };
          }
          return item2;
        });
      } else {
        const nftInfoWithOwner =
          await this.GraphqlService.getOneNFTOwnersInfo721(id);
        owners = await this.prisma.user.findMany({
          where: {
            signer: nftInfoWithOwner.erc721Tokens[0].owner.id,
          },
          select: {
            email: true,
            avatar: true,
            username: true,
            signer: true,
            publicKey: true,
          },
        });
      }
      // @ts-ignore
      nft.owners = owners;
      const sellInfo = await this.eventService.findEvents({
        nftId: nft.id,
        event: SellStatus.AskNew,
        type: nft.collection.type,
        page: 0,
        limit: 1,
      });

      const bidInfo = await this.eventService.findEvents({
        nftId: nft.id,
        event: SellStatus.Bid,
        type: nft.collection.type,
        page: (bidPage - 1) * bidListLimit,
        limit: bidListLimit,
      });
      const returnNft: NftDto = {
        ...nft,
        sellInfo: sellInfo,
        bidInfo: bidInfo,
      };
      return returnNft;
    } catch (error) {
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
            select: {
              quantity: true,
              nft: {
                select: {
                  id: true,
                  name: true,
                  ipfsHash: true,
                  traits: true,
                  createdAt: true,
                  updatedAt: true,
                  status: true,
                  tokenUri: true,
                  txCreationHash: true,
                  creator: {
                    select: {
                      id: true,
                      email: true,
                      avatar: true,
                      username: true,
                      signer: true,
                      publicKey: true,
                    },
                  },
                  collection: {
                    select: {
                      id: true,
                      txCreationHash: true,
                      name: true,
                      status: true,
                      type: true,
                      category: {
                        select: {
                          id: true,
                          name: true,
                        },
                      },
                    },
                  },
                },
              },
            },
          },
        },
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  update(id: number, updateNftDto: UpdateNftDto) {
    return `This action updates a #${id} nft`;
  }

  remove(id: number) {
    return `This action removes a #${id} nft`;
  }
}
