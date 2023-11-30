import { Injectable } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
export type TraitGeneralInfo = {
  key: string;
  count: number;
  traits: {
    value: string;
    count: number;
  }[];
};
@Injectable()
export class TraitService {
  constructor(private prisma: PrismaService) {}

  async findUniqueTraitsInCollection(
    collectionId: string,
  ): Promise<TraitGeneralInfo[]> {
    const nfts = await this.prisma.nFT.findMany({
      where: { collectionId },
      include: {
        traits: true,
      },
    });

    const traitInfo = this.summarizeTraits(nfts);
    return traitInfo;
  }

  summarizeTraits(data: any[]) {
    if (data.length === 0) {
      return [];
    }
    const traitInfo = new Map<
      string,
      { count: number; traits: Map<string, number> }
    >();

    data.forEach((nft) => {
      nft.traits.forEach((trait: any) => {
        if (!traitInfo.has(trait.trait_type)) {
          traitInfo.set(trait.trait_type, {
            count: 0,
            traits: new Map<string, number>(),
          });
        }

        const typeInfo = traitInfo.get(trait.trait_type);
        if (typeInfo) {
          typeInfo.count++;

          const valueMap = typeInfo.traits;
          if (!valueMap.has(trait.value)) {
            valueMap.set(trait.value, 0);
          }
          valueMap.set(trait.value, valueMap.get(trait.value) + 1);
        }
      });
    });

    return Array.from(traitInfo).map(([key, info]) => ({
      key: key,
      count: info.count,
      traits: Array.from(info.traits).map(([value, count]) => ({
        value,
        count,
      })),
    }));
  }
}
