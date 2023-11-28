import { Injectable, NotFoundException } from '@nestjs/common';
import { CreateUserDto } from './dto/create-user.dto';
import { UpdateUserDto } from './dto/update-user.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { User } from '@prisma/client';
import { GetAllUser } from './dto/get-all-user.dto';

@Injectable()
export class UserService {
  constructor(private readonly prisma: PrismaService) {}

  async findByPublicKey(publicKey: string) {
    const account = await this.prisma.user.findFirst({
      where: {
        publicKey: publicKey,
      },
    });
    if (!account) {
      return null;
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

  async updateProfile(input: UpdateUserDto, user: User) {
    return await this.prisma.user.update({
      where: {
        id: user.id,
      },
      data: {
        email: input.email,
        username: input.username,
        acceptedTerms: input.acceptedTerms,
      },
    });
  }
}
