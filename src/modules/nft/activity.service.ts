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
import { GraphQLClient, gql } from 'graphql-request';
import { Query } from '../../generated/graphql';
import { GetEventMarketplace } from './dto/event-marketplace.dto';
import { GetEventBase } from './dto/event-base.dto';
import { GetActivityBase } from './dto/activity-nft.dto';
import { creatorSelect } from '../../commons/definitions/Constraint.Object';

interface NullableUser {
  id: string | null;
  email: string | null;
  avatar: string | null;
  username: string | null;
  signer: string;
}

interface NullableCollection {
  id: string;
  txCreationHash: string;
  name: string;
  address: string;
  isU2U: boolean;
  shortUrl: string;
}

interface NullableNFT {
  id: string;
  u2uId: string;
  name: string;
  image: string;
  animationUrl: boolean;
}

@Injectable()
export class ActivityService {
  constructor(
    private prisma: PrismaService,
    private readonly GraphqlService: GraphQlcallerService,
  ) {}

  private readonly endpoint = process.env.SUBGRAPH_URL;
  private client = this.getGraphqlClient();
  private getGraphqlClient() {
    return new GraphQLClient(this.endpoint);
  }

  getUserData = async (signer: string) => {
    try {
      return await this.prisma.user.findFirst({
        where: { signer },
        select: creatorSelect,
      });
    } catch (error) {
      console.error(`Error fetching user data for signer ${signer}:`, error);
      throw error; // You may want to handle or log the error accordingly
    }
  };

  getNFTData = async (id: string, collectionAddress: string) => {
    try {
      return await this.prisma.nFT.findFirst({
        where: {
          OR: [
            {
              AND: [
                { u2uId: id },
                {
                  collection: {
                    address: collectionAddress,
                  },
                },
              ],
            },
            {
              AND: [
                { id },
                {
                  collection: {
                    address: collectionAddress,
                  },
                },
              ],
            },
          ],
        },
        select: {
          id: true,
          u2uId: true,
          name: true,
          image: true,
          animationUrl: true,
        },
      });
    } catch (error) {
      console.error(`Error fetching user data for signer ${id}:`, error);
      throw error; // You may want to handle or log the error accordingly
    }
  };

  async getCollectionData(address: string) {
    try {
      return this.prisma.collection.findFirst({
        where: {
          address: address,
        },
        select: {
          id: true,
          txCreationHash: true,
          name: true,
          address: true,
          status: true,
          isU2U: true,
          shortUrl: true,
          type: true,
        },
      });
    } catch (error) {
      console.error(
        `Error fetching collection data for address ${address}:`,
        error,
      );
      throw error; // You may want to handle or log the error accordingly
    }
  }

  async fetchUserData(signer: string) {
    if (signer !== ZERO_ADDR) {
      const response = await this.getUserData(signer);
      return response || {};
    }
    return signer;
  }

  async fetchActivityFromGraph(input: GetEventBase) {
    try {
      const { and, or, page, limit, type } = input;

      const { isWhereEmpty, whereClause } =
        await this.GraphqlService.formatWherecondition({
          and,
          or,
        });

      const query = gql`
        query getActivity($page: Int, $limit: Int) {
          blocks(
            ${isWhereEmpty ? '' : whereClause}
            skip: $page
            first: $limit
            orderBy: timestampt
            orderDirection: desc
          ) {
            tokenId
            from
            to
            timestampt
            quantity
            price
            id
            event
            blockNumber
            address
            quoteToken
          }
        }
      `;
      const pageCalculation = (page - 1) * limit;
      const { blocks = [] } = (await this.client.request(query, {
        pageCalculation,
        limit,
      })) as unknown as Query;
      return blocks;
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async processActivityNFTData(blocks: any[]) {
    const result = await Promise.all(
      blocks.map(async (item) => {
        const toResult = await this.fetchUserData(item?.to);
        const fromResult = await this.fetchUserData(item?.from);
        const collection = await this.getCollectionData(item?.address);
        const NFT = await this.getNFTData(item?.tokenId, item?.address);

        const to =
          item?.to !== ZERO_ADDR
            ? Object.keys(toResult).length
              ? toResult
              : ({ signer: item?.to } as NullableUser)
            : ({ signer: item?.to } as NullableUser);

        const from =
          item?.from !== ZERO_ADDR
            ? Object.keys(fromResult).length
              ? fromResult
              : ({ signer: item?.from } as NullableUser)
            : ({ signer: item?.from } as NullableUser);

        const newItem = {
          ...item,
          timestamp: item?.timestampt,
          to,
          from,
          collection: collection
            ? collection
            : ({ address: item?.address } as NullableCollection),
          NFT: NFT ? NFT : ({ u2uId: item?.tokenId } as NullableNFT),
        };

        delete newItem.address;
        delete newItem.tokenId;
        delete newItem.timestampt;
        return newItem;
      }),
    );
    return result;
  }

  async processActivityUserData(blocks: any[]) {
    const result = await Promise.all(
      blocks.map(async (item) => {
        const toResult = await this.fetchUserData(item?.to);
        const fromResult = await this.fetchUserData(item?.from);
        const collection = await this.getCollectionData(item?.address);
        const NFT = await this.getNFTData(item?.tokenId, item.address);

        const to =
          item?.to !== ZERO_ADDR
            ? Object.keys(toResult).length
              ? toResult
              : ({ signer: item?.to } as NullableUser)
            : ({ signer: item?.to } as NullableUser);

        const from =
          item?.from !== ZERO_ADDR
            ? Object.keys(fromResult).length
              ? fromResult
              : ({ signer: item?.from } as NullableUser)
            : ({ signer: item?.from } as NullableUser);

        const newItem = {
          ...item,
          timestamp: item?.timestampt,
          to,
          from,
          collection: collection
            ? collection
            : ({ address: item?.address } as NullableCollection),
          NFT: NFT ? NFT : ({ u2uId: item?.tokenId } as NullableNFT),
        };

        delete newItem.address;
        delete newItem.tokenId;
        delete newItem.timestampt;
        return newItem;
      }),
    );
    return result;
  }
}
