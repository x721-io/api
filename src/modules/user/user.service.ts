import { Injectable, NotFoundException } from '@nestjs/common';
import { CreateUserDto } from './dto/create-user.dto';
import { UpdateUserDto } from './dto/update-user.dto';
import { PrismaService } from 'src/prisma/prisma.service';

@Injectable()
export class UserService {
  constructor(private readonly prisma: PrismaService) {}

  async findByPublicKey(publicKey: string) {
    const account = await this.prisma.user.findFirst({
      where: {
        signature: publicKey
      }
    })
    if (!account) {
      throw new NotFoundException('Account not found')
    }
    return account;
  }

  async updateProfile(input: UpdateUserDto) {
    await this.prisma.user.update({
      where: {
        id: input.id
      },
      data: {
        email: input.email,
        username: input.username,
      }
    })
  }
}
