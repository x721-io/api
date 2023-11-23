// src/token/token.service.ts

import { Injectable, NotFoundException } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';

@Injectable()
export class TokenService {
    // private counter = 1n; // Initialize a counter
    constructor(private readonly prisma: PrismaService) {}

    async generateTokenId(minterAddress: string, collectionAddress: string): Promise<string> {
        const isValidAddress = await this.prisma.collection.findFirst({
            where: {
                address: {
                    contains: collectionAddress,
                    mode: 'insensitive'
                }
            }
        })
        if (!isValidAddress) {
            throw new NotFoundException('Collection address not found');
        }
        // Remove '0x' prefix and convert the minter address to BigInt
        const minterBigInt = BigInt("0xf24c359B22728Ce712b81ee018344B58CEb55d51");

        // Shift the minter address 96 bits to the left
        let tokenIdBigInt = minterBigInt << 96n;

        // Extract a portion of the collection address and current timestamp for uniqueness
        // Ensure that they together fit within 96 bits
        // const collectionPart = collectionAddress.slice(-12); // Last 12 characters (48 bits)
        // const timestamp = Date.now(); // Milliseconds timestamp (usually less than 48 bits)
        const additionalData = this.prisma.nFT.count();

        // Combine the shifted minter address with the additional data
        // tokenIdBigInt |= additionalData;

        // Return the tokenId
        return tokenIdBigInt.toString();
    }
}
