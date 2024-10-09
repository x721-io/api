import {
  HttpException,
  HttpStatus,
  Injectable,
  NotFoundException,
} from '@nestjs/common';
import { CreateOrderDto } from './dto/create-order.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { GraphQLClient } from 'graphql-request';
import { getSdk } from 'src/generated/graphql';
import { CONTRACT_TYPE, ORDERTYPE, Prisma, User } from '@prisma/client';
import OrderHeplerCommon from './helper/order.helper.service';
import { UserService } from '../user/user.service';
import {
  CollectionSelect,
  nftSelect,
  userSelect,
} from 'src/commons/definitions/Constraint.Object';

@Injectable()
export class OrderService {
  constructor(
    private prisma: PrismaService,
    private userService: UserService,
  ) {}
  private readonly endpoint = process.env.SUBGRAPH_URL;
  private client = this.getGraphqlClient();
  private getGraphqlClient() {
    return new GraphQLClient(this.endpoint);
  }
  private sdk = getSdk(this.client);

  async create(input: CreateOrderDto, user: User) {
    try {
      const checkExist = await this.prisma.order.findFirst({
        where: {
          sig: input.sig,
        },
      });
      if (checkExist) {
        throw new Error('Order already Exists');
      }
      const taker = await this.userService.fetchOrCreateUser(
        input.takerAddress,
      );
      const collection = await this.prisma.collection.findFirst({
        where: {
          address: input.collectionAddress,
        },
      });
      if (!collection) {
        throw new NotFoundException();
      }
      const nft = await this.prisma.nFT.findFirst({
        where: {
          OR: [
            { AND: [{ id: input.tokenId }, { collectionId: collection.id }] },
            {
              AND: [{ u2uId: input.tokenId }, { collectionId: collection.id }],
            },
          ],
        },
      });
      if (!nft) {
        throw new NotFoundException();
      }

      // => Check owner of assets
      if (input.orderType == ORDERTYPE.SELL) {
        const check = await this.checkOwner(
          user.signer,
          input.collectionAddress,
          nft.id,
          collection.flagExtend,
          collection.subgraphUrl,
          collection.type,
          nft.u2uId,
        );
        if (!check) {
          throw new Error('Owner NFT invalid');
        }
      }
      if (input.orderType == ORDERTYPE.BID) {
        const check = await this.checkOwner(
          taker.signer,
          input.collectionAddress,
          nft.id,
          collection.flagExtend,
          collection.subgraphUrl,
          collection.type,
          nft.u2uId,
        );
        if (!check) {
          throw new Error('Owner NFT invalid');
        }
      }

      // const listingRoot = '';
      // const proof = '';

      const dataInput: Prisma.OrderUncheckedCreateInput = {
        sig: input.sig,
        makerId: user.id,
        makeAssetType: input.makeAssetType,
        makeAssetAddress: input.makeAssetAddress,
        makeAssetValue: input.makeAssetValue,
        makeAssetId: input.makeAssetId,
        takerId: taker.id,
        takeAssetType: input.takeAssetType,
        takeAssetAddress: input.takeAssetAddress,
        takeAssetValue: input.takeAssetValue,
        takeAssetId: input.takeAssetId,
        salt: input.salt,
        start: Math.floor(input.start / 1000),
        end: Math.floor(input.end / 1000),
        orderType: input.orderType,
        // listingRoot: listingRoot,
        // proof: proof,
        tokenId: nft.id,
        collectionId: collection.id,
        price: input.price,
        priceNum: OrderHeplerCommon.weiToEther(input.price),
        netPrice: input.netPrice,
        netPriceNum: OrderHeplerCommon.weiToEther(input.netPrice),
        dataOrder: input.dataOrder,
      };

      const newOrder = await this.prisma.order.create({
        data: dataInput,
        include: {
          Maker: {
            select: userSelect,
          },
          Taker: {
            select: userSelect,
          },
          nftById: {
            select: nftSelect,
          },
        },
      });
      return newOrder;
    } catch (error) {
      console.log('Create Order', error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getDetailOrder() {
    try {
    } catch (error) {
      console.log('Create Order', error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  // ower: address owner
  // address: address collection
  // tokenId: NFT ID
  async checkOwner(
    owner: string,
    address: string,
    tokenId: string,
    flagExtend: boolean,
    subgraphUrl: string,
    type: CONTRACT_TYPE,
    u2uId: string,
  ) {
    try {
      if (flagExtend == true) {
        const checkOwner = await this.getOwnerExternal(
          subgraphUrl,
          tokenId,
          owner,
          type,
        );
        return checkOwner;
      } else {
        const checkOwner = await this.getOwnerInternal(
          u2uId ? u2uId : tokenId,
          address,
          owner,
          type,
        );
        return checkOwner;
      }
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getOwnerExternal(
    subgraphUri: string,
    tokenId: string,
    owner: string,
    type: CONTRACT_TYPE,
  ) {
    let nftInfoWithOwner;
    if (type === CONTRACT_TYPE.ERC1155) {
      nftInfoWithOwner = await OrderHeplerCommon.ownerExternal(
        subgraphUri,
        CONTRACT_TYPE.ERC1155,
        tokenId,
        owner,
      );
      return nftInfoWithOwner?.userBalances?.length > 0 ? true : false;
    } else {
      nftInfoWithOwner = await OrderHeplerCommon.ownerExternal(
        subgraphUri,
        CONTRACT_TYPE.ERC721,
        tokenId,
        owner,
      );

      return nftInfoWithOwner?.items?.[0]?.owner?.id ? true : false;
    }
  }

  async getOwnerInternal(
    tokenId: string,
    contract: string,
    owner: string,
    type: CONTRACT_TYPE,
  ) {
    let nftInfoWithOwner;
    if (type === CONTRACT_TYPE.ERC1155) {
      nftInfoWithOwner = await OrderHeplerCommon.ownersInternal1155(
        contract,
        tokenId,
        owner,
      );
      return nftInfoWithOwner?.erc1155Balances?.length > 0 ? true : false;
    } else {
      nftInfoWithOwner = await OrderHeplerCommon.ownersInternal721(
        tokenId,
        contract,
        owner,
      );
      return nftInfoWithOwner?.erc721Tokens?.length > 0 ? true : false;
    }
  }
}
