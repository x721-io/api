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
        select: {
          id: true,
          email: true,
          avatar: true,
          username: true,
          signer: true,
        },
      });
    } catch (error) {
      console.error(`Error fetching user data for signer ${signer}:`, error);
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
        const to = await this.fetchUserData(item?.to);
        const from = await this.fetchUserData(item?.from);
        const collection = await this.getCollectionData(item?.address);

        return {
          ...item,
          to: to !== ZERO_ADDR ? to : ({ signer: item?.to } as NullableUser),
          from:
            from !== ZERO_ADDR
              ? from
              : ({ signer: item?.from } as NullableUser),
          collection: collection
            ? collection
            : ({ address: item?.address } as NullableCollection),
        };
      }),
    );
    return result;
  }
}
