import { Injectable } from '@nestjs/common';
import { UpdateCommonDto } from './dto/update-common.dto';
import { create } from 'ipfs-http-client';
import OtherCommon from 'src/commons/Other.common';
import { Response } from 'express';
import * as fileType from 'file-type';
import { SearchAllDto } from './dto/search-all.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { SearchAllType } from 'src/constants/searchType.enum';
@Injectable()
export class CommonService {
  private ipfs;
  constructor(private readonly prisma: PrismaService) {
    this.ipfs = create({
      host: process.env.IPFS_URL,
      port: parseInt(process.env.IPFS_PORT),
      protocol: process.env.IPFS_PROTOCOL,
    });
  }

  async searchAll(input: SearchAllDto) {
    if (input.mode === SearchAllType.COLLECTION) {
      return this.prisma.collection.findMany({
        where: {
          OR: [
            {
              name: {
                contains: input.text,
                mode: 'insensitive',
              },
            },
            {
              symbol: {
                contains: input.text,
                mode: 'insensitive',
              },
            },
          ],
        },
      });
    } else if (input.mode === SearchAllType.USER) {
      return this.prisma.user.findMany({
        select: {
          id: true,
          signer: true,
          username: true,
          avatar: true,
          createdAt: true,
          shortLink: true,
        },
        where: {
          OR: [
            {
              username: {
                contains: input.text,
                mode: 'insensitive',
              },
            },

            {
              signer: {
                contains: input.text,
                mode: 'insensitive',
              },
            },
          ],
        },
      });
    } else if (input.mode === SearchAllType.NFT) {
      return this.prisma.nFT.findMany({
        where: {
          OR: [
            {
              name: {
                contains: input.text,
                mode: 'insensitive',
              },
            },
          ],
        },
        include: {
          collection: {
            select: {
              id: true,
              txCreationHash: true,
              name: true,
              address: true,
              metadata: true,
              shortUrl: true,
              symbol: true,
              description: true,
              status: true,
              type: true,
              categoryId: true,
              createdAt: true,
              avatar: true,
              category: {
                select: {
                  id: true,
                  name: true,
                },
              },
            },
          },
        },
      });
    }
  }
  async uploadIpfs(files: Express.Multer.File[], metadata: any) {
    try {
      const fileResults = await Promise.all(
        files.map((file) => this.ipfs.add(file.buffer)),
      );
      const fileHashes = fileResults.map((result) => result.path);
      if (metadata) {
        const metadataObject = JSON.parse(metadata);
        const updatedMetadata = { ...metadataObject, fileHashes };

        const metadataResult = await this.ipfs.add(
          JSON.stringify(updatedMetadata),
        );
        return {
          fileHashes: fileHashes,
          metadataHash: metadataResult.path,
        };
      } else {
        return { fileHashes: fileHashes };
      }
    } catch (err) {
      console.log('err: ', err);
    }
  }

  async getFromIpfs(hash: string): Promise<{ data: any; type: string }> {
    try {
      const content = [];
      for await (const chunk of this.ipfs.cat(hash)) {
        content.push(chunk);
      }
      const buffer = Buffer.concat(content);

      // Try to parse buffer as JSON
      try {
        const json = JSON.parse(buffer.toString());
        return { data: json, type: 'json' };
      } catch (e) {
        // If it's not JSON, return it as a file
        return { data: buffer, type: 'file' };
      }
    } catch (err) {
      console.error('Error retrieving content from IPFS:', err);
      throw err;
    }
  }

  async getFileFromIpfs(hash: string, res: Response) {
    try {
      const content = [];
      for await (const chunk of this.ipfs.cat(hash)) {
        content.push(chunk);
      }
      const buffer = Buffer.concat(content);
      const filetype = await fileType.fromBuffer(buffer);
      const { ext, mime } = filetype;
      const result = OtherCommon.convertBufferToFile(buffer);
      // Set response headers
      res.set({
        'Content-Type': `${mime}`,
        'Content-Disposition': `attachment; filename=${hash}.${ext}`,
      });

      // // Send the Blob as the response
      res.send(result);
    } catch (err) {
      console.error('Error retrieving content from IPFS:', err);
      throw err;
    }
  }

  findAll() {
    return `This action returns all common`;
  }

  findOne(id: number) {
    return `This action returns a #${id} common`;
  }

  update(id: number, updateCommonDto: UpdateCommonDto) {
    return `This action updates a #${id} common`;
  }

  remove(id: number) {
    return `This action removes a #${id} common`;
  }
}
