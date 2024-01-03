import { Injectable } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';
import { SellStatus } from 'src/generated/graphql';

@Injectable()
export class CollectionPriceService {
  constructor(
    private prisma: PrismaService,
    private readonly graphQL: GraphQlcallerService,
  ) {}

  async filterFloorPriceFromSubgraph(
    min?: string,
    max?: string,
  ): Promise<string[]> {
    const {
      marketEvent1155S: filterSelling1155,
      marketEvent721S: filterSelling721,
    } = await this.graphQL.getNFTSellStatus1({
      and: [{ event: SellStatus.AskNew }],
      // or: [{ from: filter.owner }, { to: filter.owner }],
    });
    const collectionsWithFloorPrice = this.getFloorPrices([
      ...filterSelling1155,
      ...filterSelling721,
    ]);
    const filteredCollection = this.filterFloorPrices(
      collectionsWithFloorPrice,
      min,
      max,
    );
    return filteredCollection.map((i) => i.contractId);
  }

  filterFloorPrices(floorPrices: any[], min?: string, max?: string) {
    return floorPrices.filter((item) => {
      const price = BigInt(item.floorPrice);
      if (min !== undefined && max !== undefined) {
        return price >= BigInt(min) && price <= BigInt(max);
      } else if (min !== undefined) {
        return price >= BigInt(min);
      } else if (max !== undefined) {
        return price <= BigInt(max);
      }
      return true;
    });
  }

  getFloorPrices(data) {
    // Create a set of unique contract IDs
    const contractIds = new Set(data.map((item) => item.nftId.contract.id));

    // Map each contract ID to its floor price
    const floorPrices = Array.from(contractIds).map((contractId) => {
      // Filter items by contract ID
      const items = data.filter(
        (item) => item.nftId.contract.id === contractId,
      );

      // Calculate the floor price for this contract
      const floorPrice = items.reduce((min, item) => {
        const price = BigInt(item.price);
        return price < min ? price : min;
      }, BigInt(items[0].price));

      return {
        contractId,
        floorPrice: floorPrice.toString(),
      };
    });

    return floorPrices;
  }

  async filterFloorPriceFromSubgraphWithoutMinMax(): Promise<any[]> {
    const {
      marketEvent1155S: filterSelling1155,
      marketEvent721S: filterSelling721,
    } = await this.graphQL.getNFTSellStatus1({
      and: [{ event: SellStatus.AskNew }],
      // or: [{ from: filter.owner }, { to: filter.owner }],
    });
    const collectionsWithFloorPrice = this.getFloorPrices([
      ...filterSelling1155,
      ...filterSelling721,
    ]);
    return collectionsWithFloorPrice.map((i) => i.contractId);
  }
}
