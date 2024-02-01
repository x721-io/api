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
interface AggregatedTrait {
  trait_type: string;
  value: string;
  count: number;
}
@Injectable()
export class TraitService {
  constructor(private prisma: PrismaService) {}

  async findUniqueTraitsInCollection(
    collectionId: string,
  ): Promise<TraitGeneralInfo[]> {
    try {
      const aggregatedTraits = await this.prisma.$queryRaw<AggregatedTrait[]>`
      SELECT 
        t.trait_type,
        t.value,
        COUNT(*) as count
      FROM 
        "Trait" t
      INNER JOIN 
        "NFT" n ON t."nftId" = n.id AND t."collectionId"::uuid = n."collectionId"
      WHERE 
        n."collectionId" = ${collectionId}::uuid
      GROUP BY 
        t.trait_type, t.value
    `;

      // Map to store the aggregated data with the structure of TraitGeneralInfo
      const traitGeneralInfoMap = new Map();

      aggregatedTraits.forEach((aggregatedTrait) => {
        if (!traitGeneralInfoMap.has(aggregatedTrait.trait_type)) {
          traitGeneralInfoMap.set(aggregatedTrait.trait_type, {
            key: aggregatedTrait.trait_type,
            count: 0, // Initialize as a number
            traits: [],
          });
        }

        const typeInfo = traitGeneralInfoMap.get(aggregatedTrait.trait_type);
        if (typeInfo) {
          const traitCount = Number(aggregatedTrait.count); // Convert BigInt to Number
          typeInfo.count += traitCount; // Now both are numbers

          // Check if this specific value has been added before
          const traitValue = typeInfo.traits.find(
            (t) => t.value === aggregatedTrait.value,
          );
          if (traitValue) {
            traitValue.count += traitCount; // Increment count if value exists
          } else {
            // Otherwise, add new trait value
            typeInfo.traits.push({
              value: aggregatedTrait.value,
              count: traitCount,
            });
          }
        }
      });

      return Array.from(traitGeneralInfoMap.values());
    } catch (err) {
      console.error('ủa: ', err);
    }
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
