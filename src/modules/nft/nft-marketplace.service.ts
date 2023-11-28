import { Injectable } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';
import { GetEventMarketplace } from './dto/event-marketplace.dto';
import { GetEventBase } from './dto/event-base.dto';

@Injectable()
export class MarketplaceService {
  constructor(
    private prisma: PrismaService,
    private readonly GraphQl: GraphQlcallerService,
  ) {}

  async findEvents(input: GetEventMarketplace) {
    const { nftId, from, to, quoteToken, event, page, limit, type } = input;
    const res = await this.GraphQl.getNFTSellStatus(
      page,
      limit,
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
    const res = await this.GraphQl.getNFTSellStatus1(page, limit, {
      and,
      or,
    });
    if (type === 'ERC1155') {
      return res.marketEvent1155S;
    } else if (type === 'ERC721') return res.marketEvent721S;
    else return [...res.marketEvent1155S, ...res.marketEvent721S];
  }
}
