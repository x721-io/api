import { Injectable } from '@nestjs/common';
import { CreateCollectionDto } from './dto/create-collection.dto';
import { UpdateCollectionDto } from './dto/update-collection.dto';
import {CollectionEntity} from './entities/collection.entity';
import { PrismaService } from 'src/prisma/prisma.service';
let moment = require("moment-timezone");
moment().tz("Asia/Ho_Chi_Minh").format();


@Injectable()
export class CollectionService {
  constructor(private prisma : PrismaService){}

  create(input: CreateCollectionDto) : Promise<CollectionEntity> {
    return this.prisma.collection.create({
      data : {         
        txCreationHash : input.txCreationHash,
        name : input.name,
        symbol : input.name,  
        description : input.name,    
        status: input.status,         
        type : input.type,          
        categoryId: input.categoryId,
      }
    })
  }

  async findAll() : Promise<CollectionEntity[]> {
    return this.prisma.collection.findMany()
  }
  
  async findOne(id: string) : Promise<CollectionEntity> {
    return this.prisma.collection.findUnique({
      where : {
        id : id
      }
    })
  }

  update(id: string, input: UpdateCollectionDto) : Promise<CollectionEntity> {
    return this.prisma.collection.update({
      where : {id : id},
      data : {
        txCreationHash : input.txCreationHash,
        name : input.name,
        symbol : input.description,
        status : input.status,
        type : input.type,
        categoryId : input.categoryId,
      }
    })
  }

  remove(id: string) : Promise<CollectionEntity> {
    return this.prisma.collection.delete({
      where : {
        id : id
      }
    })
  }
}
