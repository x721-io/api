import { Injectable } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';
import { GetEventMarketplace } from './dto/event-marketplace.dto';

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
    }
    return res.marketEvent721S;
  }
}
