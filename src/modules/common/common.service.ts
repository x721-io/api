import { Injectable, Res } from '@nestjs/common';
import { UpdateCommonDto } from './dto/update-common.dto';
import { create } from 'ipfs-http-client';
import OtherCommon from 'src/commons/Other.common';
import { Response } from 'express';
import * as fileType from 'file-type';
import { SearchAllDto } from './dto/search-all.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { SearchAllType } from 'src/constants/searchType.enum';
import { Readable } from 'stream';
import { concat as uint8ArrayConcat } from 'uint8arrays/concat';
import * as path from 'path';

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
      const filesToAdd = files.map((file) => ({
        path: file.originalname,
        content: file.buffer,
      }));
      const results = [];
      for await (const result of this.ipfs.addAll(filesToAdd, {
        wrapWithDirectory: true,
      })) {
        results.push(result);
      }

      // Assuming the last entry is the directory
      const directory = results[results.length - 1];
      if (!directory) {
        throw new Error('Failed to retrieve directory CID');
      }
      const directoryCid = directory.cid.toString();

      const fileUrls = files.map(
        (file) => `ipfs://ipfs/${directoryCid}/${file.originalname}`,
      );
      if (metadata) {
        const metadataObject = JSON.parse(metadata);
        const updatedMetadata = { ...metadataObject, fileUrls };

        const metadataResult = await this.ipfs.add(
          JSON.stringify(updatedMetadata),
        );
        return {
          fileHashes: fileUrls,
          metadataHash: metadataResult.path,
        };
      } else {
        return { fileHashes: fileUrls };
      }
    } catch (err) {
      console.log('err: ', err);
    }
  }

  async getFileFromIpfsPath(ipfsPath: string, @Res() res): Promise<void> {
    const { cid, filePath } = this.parseIpfsPath(ipfsPath);

    const fileExtension = path.extname(filePath).toLowerCase(); // Extract file extension

    // Set Content-Type based on file extension
    const contentType = this.getContentType(fileExtension);
    res.setHeader('Content-Type', contentType);

    const contentGenerator = this.ipfs.cat(`${cid}/${filePath}`);

    for await (const chunk of contentGenerator) {
      res.write(chunk);
    }

    res.end();
  }

  private getContentType(fileExtension: string): string {
    switch (fileExtension) {
      case '.jpg':
      case '.jpeg':
        return 'image/jpeg';
      case '.png':
        return 'image/png';
      case '.gif':
        return 'image/gif';
      case '.bmp':
        return 'image/bmp';
      case '.svg':
        return 'image/svg+xml';
      case '.webp':
        return 'image/webp';
      case '.pdf':
        return 'application/pdf';
      case '.mp3':
        return 'audio/mpeg';
      case '.mp4':
        return 'video/mp4';
      case '.avi':
        return 'video/x-msvideo';
      case '.wmv':
        return 'video/x-ms-wmv';
      case '.mov':
        return 'video/quicktime';
      case '.flv':
        return 'video/x-flv';
      case '.ogg':
        return 'audio/ogg';
      case '.wav':
        return 'audio/wav';
      case '.webm':
        return 'video/webm';
      case '.json':
        return 'application/json';
      case '.xml':
        return 'application/xml';
      // Add more cases for different file types as needed
      default:
        return 'application/json';
    }
  }
  // async getFromIpfs(hash: string): Promise<{ data: any; type: string }> {
  //   try {
  //     const content = [];
  //     for await (const chunk of this.ipfs.cat(hash)) {
  //       content.push(chunk);
  //     }
  //     const buffer = Buffer.concat(content);

  //     // Try to parse buffer as JSON
  //     try {
  //       const json = JSON.parse(buffer.toString());
  //       return { data: json, type: 'json' };
  //     } catch (e) {
  //       // If it's not JSON, return it as a file
  //       return { data: buffer, type: 'file' };
  //     }
  //   } catch (err) {
  //     console.error('Error retrieving content from IPFS:', err);
  //     throw err;
  //   }
  // }

  // async getFileFromIpfs(hash: string, res: Response) {
  //   try {
  //     const content = [];
  //     for await (const chunk of this.ipfs.cat(hash)) {
  //       content.push(chunk);
  //     }
  //     const buffer = Buffer.concat(content);
  //     const filetype = await fileType.fromBuffer(buffer);
  //     const { ext, mime } = filetype;
  //     const result = OtherCommon.convertBufferToFile(buffer);
  //     // Set response headers
  //     res.set({
  //       'Content-Type': `${mime}`,
  //       'Content-Disposition': `attachment; filename=${hash}.${ext}`,
  //     });

  //     // // Send the Blob as the response
  //     res.send(result);
  //   } catch (err) {
  //     console.error('Error retrieving content from IPFS:', err);
  //     throw err;
  //   }
  // }

  async retrieveFilesFromDirectory(
    directoryCid: string,
  ): Promise<{ filename: string; content: Buffer }[]> {
    const files = [];

    for await (const file of this.ipfs.ls(directoryCid)) {
      if (file.type === 'file') {
        // Stream file content
        const contentStream = [];
        for await (const chunk of this.ipfs.cat(file.cid)) {
          contentStream.push(chunk);
        }

        // Concatenate the chunks into a single Buffer
        const contentBuffer = Buffer.from(uint8ArrayConcat(contentStream));
        files.push({ filename: file.name, content: contentBuffer });
      }
    }

    return files;
  }

  private parseIpfsPath(ipfsPath: string): { cid: string; filePath: string } {
    const pathParts = ipfsPath.replace('ipfs://ipfs/', '').split('/');
    const cid = pathParts[0];
    const filePath = pathParts.slice(1).join('/');

    return { cid, filePath };
  }
}
