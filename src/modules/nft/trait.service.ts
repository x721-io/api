import { Injectable } from "@nestjs/common";
import { PrismaService } from "src/prisma/prisma.service";

@Injectable()
export class TraitService {
    constructor(private prisma: PrismaService) {}

    async findUniqueTraitsInCollection(collectionId: string): Promise<string[]> {
        const nfts = await this.prisma.nFT.findMany({
            where: {collectionId},
            include: {
                traits: true,
            }
        })

        const traitTypes = new Set<string>();
        nfts.forEach(nft => {
            nft.traits.forEach(trait => {
              traitTypes.add(trait.trait_type);
            });
          });
        return Array.from(traitTypes);
    }
}