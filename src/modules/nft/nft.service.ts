import { CreateNftDto } from './dto/create-nft.dto';
import { UpdateNftDto } from './dto/update-nft.dto';
import { Prisma, TX_STATUS, User } from '@prisma/client'
import { PrismaService } from 'src/prisma/prisma.service';
import { NftDto } from './dto/nft.dto';
import { Injectable, HttpException, HttpStatus, NotFoundException } from '@nestjs/common';
import { validate as isValidUUID } from 'uuid';
import { Redis } from 'src/database';
import { GetAllNftDto } from './dto/get-all-nft.dto';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';

@Injectable()
export class NftService {
  constructor(private prisma: PrismaService, private readonly GraphqlService: GraphQlcallerService) { }
  async create(input: CreateNftDto, user: User): Promise<NftDto> {
    try {
      let checkExist = await this.prisma.nFT.findFirst({
        where: {
          OR: [
            { txCreationHash: input.txCreationHash },
            { name: input.name },
            { id: input.id }
          ]
        }
      });

      // if (!isValidUUID(input.creatorId)) {
      //   throw new Error('Invalid Creator ID. Please try again !');
      // }

      if (!isValidUUID(input.collectionId)) {
        throw new Error('Invalid Collection ID. Please try again !');
      }

      if (checkExist) {
        throw new Error('Transaction hash or name or ID already exists')
      }

      let nft = await this.prisma.nFT.create({
        data: {
          id: input.id,
          name: input.name,
          ipfsHash: input.name,
          traits: {
            create: input.traits,
          },
          status: TX_STATUS.PENDING,
          tokenUri: input.tokenUri,
          txCreationHash: input.txCreationHash,
          creatorId: user.id,
          collectionId: input.collectionId
        },
        include: {
          traits: true,
          collection: true,
        }
      });
      await this.prisma.userNFT.create({
        data: {
          userId: user.id,
          nftId: input.id,
        }
      })
      await Redis.publish('nft-channel', JSON.stringify({ txCreation: nft.txCreationHash, type: nft.collection.type }))
      return nft;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async findAll(filter: GetAllNftDto): Promise<NftDto[]> {
    try {
      let traitsConditions = [];

      // TODO: if price and status are included, then use subgraph as main source and use other to eliminate 

      if (filter.traits) {
        traitsConditions = filter.traits.map(trait => ({
          traits: {
            some: {
              trait_type: trait.trait_type,
              ...(trait.value && { value: trait.value }),
              ...(trait.display_type && { display_type: trait.display_type }),
            },
          },
        }));
      }
      let whereCondition: Prisma.NFTWhereInput = {
        AND: traitsConditions,
        ...(filter.creatorAddress && { creator: {
          publicKey: filter.creatorAddress
        }}),
        ...(filter.collectionAddress && { collection: {
          address: filter.collectionAddress
        }}),
        ...(filter.name && { name: filter.name})
      }
      const nfts = await this.prisma.nFT.findMany({
        where: whereCondition,
        include: {
          creator: {
            select: {
              id: true,
              email: true,
              avatar: true,
              username: true,
              signature: true,
              signer: true,
              publicKey: true,
              acceptedTerms: true,
            }
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
                  name: true
                }
              }
            }
          },
          traits: true,
        }
      })
      // const response = await this.GraphqlService.getNFTsHistory(nfts[0].id, 10)
      // console.log(response.marketEvent721S)
      // const lastNFTStatus = response.marketEvent721S[response.marketEvent721S.length - 1];
      return nfts;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
  
  async findOne(id: string): Promise<NftDto> {
    try {
      let checkExist = await this.prisma.nFT.findFirst({ where: { id: id } });
      if (!checkExist) {
        throw new NotFoundException()
      }
      return this.prisma.nFT.findUnique({
        where: {
          id: id
        },
        include: {
          owners: {
            select: {
              userId: true,
              nftId: true,
              quantity: true,
              user: {
                select: {
                  email: true,
                  avatar: true,
                  username: true,
                  signature: true,
                  signedMessage: true,
                  signer: true,
                  publicKey: true,
                  signDate: true,
                  acceptedTerms: true,
                  createdAt: true
                }
              }
            }
          },
          creator: {
            select: {
              id: true,
              email: true,
              avatar: true,
              username: true,
              signature: true,
              signedMessage: true,
              signer: true,
              publicKey: true,
              signDate: true,
              acceptedTerms: true,
              createdAt: true
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
                  name: true
                }
              }
            }
          },
          traits: true,
        }
      })
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async findNFTByUserID(id: string): Promise<any[]> {
    try {
      if (!isValidUUID(id)) {
        throw new Error('Invalid User. Please try again !');
      }
      let checkExist = await this.prisma.user.findFirst({ where: { id: id } });
      if (!checkExist) {
        throw new NotFoundException()
      }
      return this.prisma.user.findMany({
        where: {
          id: id
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
                      signature: true,
                      signedMessage: true,
                      signer: true,
                      publicKey: true,
                      signDate: true,
                      acceptedTerms: true,
                      createdAt: true
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
                          name: true
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      })
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
