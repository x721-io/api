import {
  HttpException,
  HttpStatus,
  Injectable,
  NotFoundException,
} from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';
import {
  GetEventMarketplace,
  GetEventOrder,
} from './dto/event-marketplace.dto';
import { GetEventBase } from './dto/event-base.dto';
import { NftService } from './nft.service';
import {
  nftSelect,
  userSelect,
} from 'src/commons/definitions/Constraint.Object';
import { timestamp } from 'rxjs';
import { ORDERTYPE } from '@prisma/client';
import { start } from 'repl';

export interface UserInterface {
  id: string;
  email: string;
  avatar: string;
  username: string;
  accountStatus: string;
  verifyEmail: string;
  signer: string;
}

interface OfferInterface {
  id: string;
  index: number;
  sig: string;
  makerId: string;
  makeAssetType: number;
  makeAssetAddress: string;
  makeAssetValue: string;
  makeAssetId: string;
  takerId: string | null;
  takeAssetType: number;
  takeAssetAddress: string;
  takeAssetValue: string;
  takeAssetId: string;
  salt: string;
  start: number;
  end: number;
  dataOrder: string;
  orderStatus: string;
  orderType: string;
  root: string;
  proof: [];
  tokenId: string;
  collectionId: string;
  quantity: number;
  price: string;
  priceNum: number;
  netPrice: string;
  netPriceNum: number;
  createAt: string;
  updatedAt: string;
  quoteToken: string;
  filled: number;
  Maker: UserInterface;
  Taker: UserInterface;
}

@Injectable()
export class MarketplaceService {
  constructor(
    private prisma: PrismaService,
    private readonly GraphQl: GraphQlcallerService,
  ) {}

  async findEvents(input: GetEventMarketplace) {
    const {
      nftId,
      from,
      to,
      quoteToken,
      event,
      page,
      limit,
      type,
      contractAddress,
    } = input;
    const res = await this.GraphQl.getNFTSellStatus(
      page,
      limit,
      contractAddress,
      nftId,
      from,
      to,
      quoteToken,
      event,
    );
    if (type === 'ERC1155') {
      return res.marketEvent1155S;
    } else if (type === 'ERC721') return res.marketEvent721S;
    else return [...res.marketEvent1155S, ...res.marketEvent721S];
  }

  async findEvents1(input: GetEventBase) {
    const { and, page, limit, or, type } = input;
    const res = await this.GraphQl.getNFTSellStatus1(
      {
        and,
        or,
      },
      page,
      limit,
    );
    const transformErc1155Event = await Promise.all(
      res.marketEvent1155S.map(async (item) => {
        if (!item.nftId) {
          return null;
        }
        const nft = await this.findNftByU2UId(
          item.nftId.contract.id,
          item.nftId.tokenId,
        );
        if (nft) {
          item.nftId.tokenId = nft.id;
          return item;
        }
        return item;
      }),
    );

    const transformErc721Event = await Promise.all(
      res.marketEvent721S.map(async (item) => {
        if (!item.nftId) {
          return null;
        }
        const nft = await this.findNftByU2UId(
          item.nftId.contract.id,
          item.nftId.tokenId,
        );
        if (nft) {
          item.nftId.tokenId = nft.id;
          return item;
        }
        return item;
      }),
    );
    if (type === 'ERC1155') {
      return transformErc1155Event;
    } else if (type === 'ERC721') {
      return transformErc721Event;
    } else return [...transformErc1155Event, ...transformErc721Event];
  }

  async findNftByU2UId(collectionAddress: string, u2uId: string) {
    console.log(collectionAddress);
    const collection = await this.prisma.collection.findUnique({
      where: {
        address: collectionAddress,
      },
    });
    // if (!collection) {
    //   throw new NotFoundException('Collection not found');
    // }
    if (collection) {
      const nft = await this.prisma.nFT.findFirst({
        where: {
          u2uId,
          collectionId: collection.id,
        },
      });
      // if (!nft) throw new NotFoundException('NFT not found');
      return nft;
    } else {
      return null;
    }
  }

  async findOrder(input: GetEventOrder) {
    try {
      const collection = await this.prisma.collection.findFirst({
        where: {
          address: input.contractAddress,
        },
      });

      if (!collection) {
        throw new NotFoundException();
      }
      const currentDate = Math.floor(Date.now() / 1000);
      const offer = await this.prisma.order.findMany({
        where: {
          collectionId: collection.id,
          tokenId: input.nftId,
          orderStatus: input.status,
          orderType: {
            in: input.event,
          },
          start: {
            lte: currentDate,
          },
          end: {
            gte: currentDate,
          },
        },
        skip: (input.page - 1) * input.limit,
        take: input.limit,
        orderBy: {
          priceNum: 'desc',
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

      const result = offer.map((item) => {
        const format = this.formatDataOffer(item as any);
        return {
          ...format,
        };
      });
      return result;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  formatDataOffer(input: OfferInterface) {
    const date = new Date(input.createAt);
    const timestampConvert = date.getTime();
    const format = {
      // ...input,
      id: input.id,
      index: input.index,
      sig: input.sig,
      orderType: input.orderType,
      orderStatus: input.orderStatus,
      // quoteToken:
      //   input.orderType == ORDERTYPE.BID
      //     ? input.makeAssetAddress
      //     : input.takeAssetAddress,
      price: input.price,
      priceNum: input.priceNum,
      netPrice: input.netPrice,
      netPriceNum: input.netPriceNum,
      quanity: input.quantity,
      createAt: input.createAt,
      start: input.start,
      end: input.end,
      quoteToken: input.quoteToken,
      filled: input.filled,
      timestamp: timestampConvert,
      Taker: input.Taker, //to
      Maker: input.Maker, // from
    };
    return format;
  }
}
