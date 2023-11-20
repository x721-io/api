import { CreateNftDto } from './dto/create-nft.dto';
import { UpdateNftDto } from './dto/update-nft.dto';
import { CONTRACT_TYPE, TX_STATUS, User } from '@prisma/client'
import { PrismaService } from 'src/prisma/prisma.service';
import { NftDto } from './dto/nft.dto';
import { Injectable, HttpException, HttpStatus, NotFoundException } from '@nestjs/common';
import { validate as isValidUUID } from 'uuid';
import { Redis } from 'src/database';

@Injectable()
export class NftService {
  constructor(private prisma: PrismaService) { }
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

      if (!isValidUUID(input.creatorId)) {
        throw new Error('Invalid Creator ID. Please try again !');
      }

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
          traits: input.traits,
          status: TX_STATUS.PENDING,
          tokenUri: input.tokenUri,
          txCreationHash: input.txCreationHash,
          creatorId: input.creatorId,
          collectionId: input.collectionId
        }
      });
      await this.prisma.userNFT.create({
        data: {
          userId: user.id,
          nftId: input.id,
        }
      })
      const collectionType = await this.prisma.collection.findUnique({
        where: {
          id: input.collectionId,
        },
      })
      await Redis.publish('nft-channel', JSON.stringify({ txCreation: nft.txCreationHash, type: collectionType.type }))
      return nft;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async findAll(): Promise<NftDto[]> {
    try {
      return this.prisma.nFT.findMany({
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
          }
        }
      })
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
          }
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
