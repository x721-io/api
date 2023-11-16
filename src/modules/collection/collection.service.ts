import { Injectable } from '@nestjs/common';
import { CreateCollectionDto } from './dto/create-collection.dto';
import { UpdateCollectionDto } from './dto/update-collection.dto';
import { Redis } from 'src/database';

@Injectable()
export class CollectionService {
  async create() {
    await Redis.publish('collection-channel', JSON.stringify({ txCreation: '0xdcc0a5122e1cd7a47b1eb32ff7ad82c9e60d694382499c286ba39cc8c664a615'}))
  }
}
