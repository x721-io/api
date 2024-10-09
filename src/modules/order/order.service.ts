import { id } from './../../../node_modules/aws-sdk/clients/datapipeline.d';
import { response } from 'express';
import {
  HttpException,
  HttpStatus,
  Injectable,
  NotFoundException,
} from '@nestjs/common';
import { CreateBulkDto, CreateSingleDto } from './dto/create-order.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { GraphQLClient } from 'graphql-request';
import { getSdk } from 'src/generated/graphql';
import {
  CONTRACT_TYPE,
  ORDERSTATUS,
  ORDERTYPE,
  Prisma,
  User,
} from '@prisma/client';
import OrderHeplerCommon from './helper/order.helper.service';
import { UserService } from '../user/user.service';
import {
  CollectionSelect,
  nftSelect,
  userSelect,
} from 'src/commons/definitions/Constraint.Object';
import { ethers } from 'ethers';
import { ActionOrderDto, VerifyOrderDto } from './dto/get-order.dto';
import { abi as exchangeABI } from 'abis/Exchange.json';
import { validate as isValidUUID } from 'uuid';
import { encodeAbiParameters, keccak256 } from 'viem';
import { MerkleTree } from 'src/commons/MerkleTree.common';

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

  private readonly endpointStaking = process.env.SUBGRAPH_URL_STAKING;
  private provider = new ethers.JsonRpcProvider(process.env.RPC_URL);

  async createSingle(input: CreateSingleDto, user: User) {
    try {
      const userTaker = await this.prisma.user.findFirst({
        where: {
          signer: input.taker.toLowerCase(),
        },
      });
      const collectionAddress =
        input.orderType == ORDERTYPE.BID
          ? input.takeAssetAddress
          : input.makeAssetAddress;
      const tokenId =
        input.orderType == ORDERTYPE.BID
          ? input.takeAssetId
          : input.makeAssetId;
      const quoteToken =
        input.orderType == ORDERTYPE.BID
          ? input.makeAssetAddress
          : input.takeAssetAddress;
      const quanity =
        input.orderType == ORDERTYPE.BID
          ? input.takeAssetValue
          : input.makeAssetValue;
      const { check, collection, nft } = await this.validateNftOwnership(
        collectionAddress,
        user.signer,
        userTaker ? userTaker.signer : null,
        input.orderType,
        tokenId,
      );
      if (!check) {
        throw new Error('Owner NFT invalid');
      }
      const dataInput: Prisma.OrderUncheckedCreateInput = {
        sig: input.sig,
        makerId: user.id,
        makeAssetType: input.makeAssetType,
        makeAssetAddress: input.makeAssetAddress,
        makeAssetValue: input.makeAssetValue,
        makeAssetId: input.makeAssetId,
        takerId: userTaker ? userTaker.id : null,
        takeAssetType: input.takeAssetType,
        takeAssetAddress: input.takeAssetAddress,
        takeAssetValue: input.takeAssetValue,
        takeAssetId: input.takeAssetId,
        salt: input.salt,
        start: parseInt(`${input.start}`),
        end: parseInt(`${input.end}`),
        orderType: input.orderType,
        tokenId: nft.id,
        collectionId: collection.id,
        price: input.price,
        quantity: parseInt(quanity),
        priceNum: OrderHeplerCommon.weiToEther(input.price),
        netPrice: input.netPrice,
        netPriceNum: OrderHeplerCommon.weiToEther(input.netPrice),
        quoteToken: quoteToken,
        index: input.index,
        proof: input.proof,
        root: input.root,
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
      const statusCode = error?.response?.statusCode || HttpStatus.BAD_REQUEST;
      throw new HttpException(`Create Single: ${error.message}`, statusCode);
    }
  }

  async generateMerkleTree(input: CreateBulkDto, user: User) {
    try {
      const endcodeOrder = input?.orders.map((ele, index) => {
        return keccak256(
          encodeAbiParameters(
            [
              { name: 'index', type: 'uint16' },
              { name: 'maker', type: 'address' },
              { name: 'makeAssetAddress', type: 'address' },
              { name: 'makeAssetId', type: 'uint256' },
              { name: 'makeAssetValue', type: 'uint256' },
              { name: 'makeAssetType', type: 'uint256' },
              { name: 'taker', type: 'address' },
              { name: 'takeAssetAddress', type: 'address' },
              { name: 'takeAssetId', type: 'uint256' },
              { name: 'takeAssetValue', type: 'uint256' },
              { name: 'takeAssetType', type: 'uint256' },
            ],
            [
              index,
              user.signer as any,
              ele.makeAssetAddress as any,
              BigInt(ele.makeAssetId),
              BigInt(ele.makeAssetValue),
              BigInt(ele.makeAssetType),
              ele.taker as any,
              ele.takeAssetAddress as any,
              BigInt(ele.takeAssetId),
              ele.takeAssetValue as any,
              BigInt(ele.takeAssetType),
            ],
          ),
        );
      });

      const merkeleTree = new MerkleTree(endcodeOrder);
      merkeleTree.generateTree();

      return {
        root: merkeleTree?.root,
        proof: merkeleTree?.proof,
      };
    } catch (error) {
      const statusCode = error?.response?.statusCode || HttpStatus.BAD_REQUEST;
      throw new HttpException(
        `Generate Merkle Tree: ${error.message}`,
        statusCode,
      );
    }
  }

  async createBulk(input: CreateBulkDto, user: User) {
    try {
      const { orders = [] } = input;
      if (orders && orders.length <= 0) {
        return;
      }
      const resultBulk = await Promise.all(
        orders.map(async (item) => {
          const data = await this.createSingle(item, user);
          return data;
        }),
      );
      return resultBulk;
    } catch (error) {
      const statusCode = error?.response?.statusCode || HttpStatus.BAD_REQUEST;
      throw new HttpException(`Check order: ${error.message}`, statusCode);
    }
  }

  // ower: address owner
  // address: address collection
  // tokenId: NFT ID
  async findOwnerNFT(
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

  async verifyOrder(input: VerifyOrderDto, user: User) {
    try {
      const { checkOwner, order } = await this.validateOrderAndOwner(
        input,
        user.signer,
      );
      if (!checkOwner) {
        throw new Error('Owner NFT invalid');
      }
      return order;
    } catch (error) {
      throw new HttpException(
        `Error verifying order: ${error.message}`,
        error?.response?.statusCode
          ? error?.response?.statusCode
          : HttpStatus.BAD_REQUEST,
      );
    }
  }

  async create(input: VerifyOrderDto, user: User) {
    try {
      const { checkOwner, order } = await this.validateOrderAndOwner(
        input,
        user.signer,
      );
      if (!checkOwner) {
        throw new Error('Owner NFT invalid');
      }
      return order;
    } catch (error) {
      throw new HttpException(
        `Error verifying order: ${error.message}`,
        error?.response?.statusCode
          ? error?.response?.statusCode
          : HttpStatus.BAD_REQUEST,
      );
    }
  }

  async validateOrderAndOwner(input: VerifyOrderDto, signer: string) {
    try {
      const currentDate = Math.floor(Date.now() / 1000);
      const order = await this.prisma.order.findFirst({
        where: {
          AND: [
            { index: input.index },
            { sig: input.sig },
            { orderStatus: ORDERSTATUS.OPEN },
            {
              start: {
                lte: currentDate,
              },
              end: {
                gte: currentDate,
              },
            },
          ],
        },
        include: {
          Maker: {
            select: userSelect,
          },
          Taker: {
            select: userSelect,
          },
        },
      });

      if (!order) {
        throw new NotFoundException('Order not found');
      }

      const checkOwner = await this.checkNftOwner(
        order.collectionId,
        order.tokenId,
        signer,
      );

      return { checkOwner, order };
    } catch (error) {
      const statusCode = error?.response?.statusCode || HttpStatus.BAD_REQUEST;
      throw new HttpException(`Check order: ${error.message}`, statusCode);
    }
  }

  async checkNftOwner(collectionId: string, tokenId: string, signer: string) {
    try {
      const { collection, nft } = await this.findNFTAndCollection(
        collectionId,
        tokenId,
      );

      const checkOwner = await this.findOwnerNFT(
        signer,
        collection.address,
        nft.id,
        collection.flagExtend,
        collection.subgraphUrl,
        collection.type,
        nft.u2uId,
      );

      return { checkOwner, collection, nft };
    } catch (error) {
      const statusCode = error?.response?.statusCode || HttpStatus.BAD_REQUEST;
      throw new HttpException(`Check NFT Owner: ${error.message}`, statusCode);
    }
  }

  async actionOrder(input: ActionOrderDto, user: User) {
    try {
      const { tx } = input;
      const transaction = await this.provider.getTransaction(tx);

      if (!transaction) {
        throw new NotFoundException('Transaction not found');
      }

      const iface = new ethers.Interface(exchangeABI);
      const { data: inputData, to, from } = transaction;
      const decodedInput = iface.parseTransaction({ data: inputData });

      const sig = decodedInput?.args[0]?.[0]?.[10];
      const index = decodedInput?.args[0]?.[0]?.[13];

      if (!sig || !index) {
        throw new Error('Invalid input data');
      }

      // Validate contract address
      if (to?.toLowerCase() !== process.env.MATCH_ORDER.toLowerCase()) {
        throw new Error('Contract is invalid');
      }

      // Validate owner
      if (from?.toLowerCase() !== user.signer?.toLowerCase()) {
        throw new Error('Owner is invalid');
      }

      // Check for existing order
      const order = await this.prisma.order.findFirst({
        where: {
          index: parseInt(index),
          sig,
          orderStatus: ORDERSTATUS.OPEN,
        },
      });

      if (!order) {
        throw new NotFoundException('Order not found');
      }

      // Update order status to PENDING
      return await this.prisma.order.update({
        where: {
          sig_index: {
            index: parseInt(index),
            sig,
          },
        },
        data: { orderStatus: ORDERSTATUS.PENDING },
      });
    } catch (error) {
      console.error('Error action order:', error);
      const statusCode = error?.response?.statusCode || HttpStatus.BAD_REQUEST;
      throw new HttpException(
        `Error action order: ${error.message}`,
        statusCode,
      );
    }
  }

  async findNFTAndCollection(collectionId: string, tokenId: string) {
    try {
      const whereInput: Prisma.CollectionWhereInput = isValidUUID(collectionId)
        ? { id: collectionId } // It's a valid UUID, so match against the id field
        : { address: collectionId };
      const collection = await this.prisma.collection.findFirst({
        where: whereInput,
      });
      if (!collection) {
        throw new NotFoundException();
      }
      const nft = await this.prisma.nFT.findFirst({
        where: {
          OR: [
            { AND: [{ id: tokenId }, { collectionId: collection.id }] },
            {
              AND: [{ u2uId: tokenId }, { collectionId: collection.id }],
            },
          ],
        },
      });
      if (!nft) {
        throw new NotFoundException();
      }
      return { collection, nft };
    } catch (error) {
      const statusCode = error?.response?.statusCode || HttpStatus.BAD_REQUEST;
      throw new HttpException(
        `Error findNFTAndCollection order: ${error.message}`,
        statusCode,
      );
    }
  }

  async validateNftOwnership(
    collectionId: string,
    makerAddress: string,
    takerAddress: string | null,
    orderType: ORDERTYPE,
    tokenId: string,
  ) {
    try {
      const address =
        orderType === ORDERTYPE.SINGLE || orderType === ORDERTYPE.BULK
          ? makerAddress
          : takerAddress;
      const { checkOwner, collection, nft } = await this.checkNftOwner(
        collectionId,
        tokenId,
        address,
      );
      return { check: checkOwner, collection, nft };
    } catch (error) {
      const statusCode = error?.response?.statusCode || HttpStatus.BAD_REQUEST;
      throw new HttpException(
        `Error validateNftOwnership: ${error.message}`,
        statusCode,
      );
    }
  }
}
