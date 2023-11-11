import { Injectable } from '@nestjs/common';
import { CreateCommonDto } from './dto/create-common.dto';
import { UpdateCommonDto } from './dto/update-common.dto';
import { create } from 'ipfs-http-client';

@Injectable()
export class CommonService {
  private ipfs;
  constructor() {
    this.ipfs = create({ host: "https://testnet-ipfs.uniultra.xyz", protocol: 'https'})
  }
  async uploadIpfs(files: Express.Multer.File[], metadata: any) {
    const fileResults = await Promise.all(
      files.map(file => this.ipfs.add(file.buffer))
    );

    const fileHashes = fileResults.map(result => result.path);
    const updatedMetadata = { ...metadata, fileHashes };

    const metadataResult = await this.ipfs.add(JSON.stringify(updatedMetadata));

    return {
      fileHashes: fileHashes,
      metadataHash: metadataResult.path
    };
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
