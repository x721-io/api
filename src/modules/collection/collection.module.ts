import { Module } from '@nestjs/common';
import { CollectionService } from './collection.service';
import { CollectionController } from './collection.controller';
import { PrismaService } from 'src/prisma/prisma.service';
import { TraitService } from '../nft/trait.service';

@Module({
  providers: [CollectionService , PrismaService, TraitService],
  controllers: [CollectionController]
})
export class CollectionModule {}
