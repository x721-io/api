import { Injectable, NotFoundException } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';
import { GetEventMarketplace } from './dto/event-marketplace.dto';
import { GetEventBase } from './dto/event-base.dto';
import { NftService } from './nft.service';

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
}
