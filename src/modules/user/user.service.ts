import { Injectable, NotFoundException } from '@nestjs/common';
import { CreateUserDto } from './dto/create-user.dto';
import { UpdateUserDto } from './dto/update-user.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { User } from '@prisma/client';

@Injectable()
export class UserService {
  constructor(private readonly prisma: PrismaService) {}

  async findByPublicKey(publicKey: string) {
    const account = await this.prisma.user.findFirst({
      where: {
        publicKey: publicKey
      }
    })
    if (!account) {
      return null;
    }
    return account;
  }

  async findOne(id: number) {
    const user = await this.prisma.user.findUnique({
      where: {
        id,
      },
    });
    if (!user) {
      throw new NotFoundException(`User #${id} not found`);
    }
    return user;
  }

  async updateProfile(input: UpdateUserDto, user: User) {
    return await this.prisma.user.update({
      where: {
        id: user.id
      },
      data: {
        email: input.email,
        username: input.username,
      }
    })
  }
}
