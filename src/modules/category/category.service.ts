import { Injectable , HttpException, HttpStatus, NotFoundException } from '@nestjs/common';
import { CreateCategoryDto } from './dto/create-category.dto';
import { UpdateCategoryDto } from './dto/update-category.dto';
import { TX_STATUS, User } from '@prisma/client'
import { PrismaService } from 'src/prisma/prisma.service';

@Injectable()
export class CategoryService {
  constructor(private prisma: PrismaService) { }
  async create(input: CreateCategoryDto , user : User) {  
    try{
      return this.prisma.category.create({
        data : {
          name : input.name
        }
      })
    }catch(error){
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async findAll() {
    try{
      return this.prisma.category.findMany();
    }catch(error){
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async findOne(id: number) {
    try{
      if(!id){
        throw new Error('Invalid ID. Please try again !');
      }
      let checkExist = await this.prisma.category.findFirst({where : {id : Number(id)}});
      if(!checkExist){
        throw new NotFoundException();
      }
      return this.prisma.category.findUnique({
        where : {
          id : Number(id)
        }
      })
    }catch(error){
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async update(id: number, input: UpdateCategoryDto , user : User) {
    try{
      if(!id){
        throw new Error('Invalid ID. Please try again !');
      }
      let checkExist = await this.prisma.category.findFirst({where : {id : Number(id)}});
      if(!checkExist){
        throw new NotFoundException();
      }
      return this.prisma.category.update({
        where : {
          id : Number(id)
        },
        data : {
          name : input.name
        }
      })

    }catch(error){
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  remove(id: number) {
    return `This action removes a #${id} category`;
  }
}
