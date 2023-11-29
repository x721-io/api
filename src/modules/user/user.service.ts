import { 
  Injectable, 
  NotFoundException,
  HttpException,
  HttpStatus
 } from '@nestjs/common';
import { CreateUserDto } from './dto/create-user.dto';
import { UpdateUserDto } from './dto/update-user.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { User } from '@prisma/client';
import { GetAllUser } from './dto/get-all-user.dto';
import  {UserEntity} from './entities/user.entity';
import { validate as isValidUUID } from 'uuid'

@Injectable()
export class UserService {
  constructor(private readonly prisma: PrismaService) {}

    // Remove few prop secret
  private minifyUserObject(user: any): any {
    const propertiesToRemove = ['signature', 'signer', 'signedMessage'];
    const minifiedUser = { ...user };
    for (const property in minifiedUser) {
      if (propertiesToRemove.includes(property)) {
        delete minifiedUser[property];
      }
    }
    return minifiedUser;
  }

  async findByPublicKey(publicKey: string) : Promise<any> {
    const account = await this.prisma.user.findFirst({
      where: {
        publicKey: publicKey,
      },
    });
    if (!account) {
      return null
    }
    return account;
  }

  async findOne(id: string) {
    const user = await this.prisma.user.findFirst({
      where: {
        id,
      },
    });
    if (!user) {
      throw new NotFoundException(`User #${id} not found`);
    }
    return user;
  }

  async findOneByWallet(address: string) {
    const user = await this.prisma.user.findUnique({
      where: {
        signer: address.toLowerCase(),
      },
    });
    if (!user) {
      throw new NotFoundException(`User #${address} not found`);
    }
    return user;
  }

  async findAll(filter: GetAllUser) {
    const limit = (filter.limit || 12) as number;
    const cursor = filter.cursor;
    // @ts-ignore
    const take: number = limit && limit > 0 ? parseInt(limit) + 1 : 13;

    const users = await this.prisma.user.findMany({
      orderBy: {
        createdAt: filter.order,
      },
      where: {
        username: { not: null },
      },
      take: take,
      cursor: cursor ? { id: cursor } : undefined,
      skip: cursor ? 1 : 0,
    });

    let nextCursor: string | null = null;
    if (users.length > limit) {
      const nextUser = users.pop();
      nextCursor = nextUser.id;
    }
    return { users, nextCursor };
  }


  async getProfileWithShortLinkOrIdUser(input: string, user: any) {
    try {
      let currentUserId = user?.id;
      let result: any = {};
  
      if (!isValidUUID(input)) {
        result = await this.prisma.user.findFirst({
          where: { OR : [{shortLink: input}, {signer: input.toLowerCase()}] },
        });
      } else {
        result = await this.prisma.user.findFirst({
          where: {  id: input },
        });
      }
  
      let response = (!currentUserId || currentUserId !== result.id)
          ? this.minifyUserObject(result)
          : result;
  
      return response;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async updateProfile(input: UpdateUserDto, user: User) {
    try{
      let updateData = {
        email: input.email,
        username: input.username,
        acceptedTerms: input.acceptedTerms,
        bio: input.bio,
        facebookLink: input.facebookLink,
        twitterLink: input.twitterLink,
        telegramLink: input.telegramLink,
        discordLink: input.discordLink,
        webURL: input.webURL,
        coverImage: input.coverImage,
        shortLink : input.shortLink
      };

      if(input && input.shortLink){
        let checkExistShortLink = await this.prisma.user.findFirst({
          where: {AND: [
            {id : { not: user.id }},
            {shortLink : input.shortLink}
          
          ]}
        })
        if(checkExistShortLink){
          throw new Error('Short Link already exists');
        } 
      }else{
        delete updateData.shortLink
      }
      
      let response = await this.prisma.user.update({
          where: {
            id: user.id
          },
          data: updateData
        })
        return response;
    }catch(error){
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
}
