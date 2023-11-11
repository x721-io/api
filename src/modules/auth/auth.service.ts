import { Injectable } from '@nestjs/common';
import { User } from '../user/entities/user.entity'; // import your User entity
import { UserService } from '../user/user.service';
import { ethers } from 'ethers';
import { loginDto } from './dto/login.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { LOGIN_MESSAGE } from 'src/constants/web3Const/messages';

@Injectable()
export class AuthService {
  constructor(private userService: UserService, private readonly prisma: PrismaService) {}

  async validateUser(validateInfo: loginDto): Promise<User> {
    const loginMessage = LOGIN_MESSAGE(validateInfo.date)
    const isSignatureValid = this.isSignatureValid(loginMessage, validateInfo.signature, validateInfo.publicKey);
    
    if (!isSignatureValid) {
      return null;
    }

    const user = await this.userService.findByPublicKey(validateInfo.publicKey);
    if (user && user.signer === validateInfo.signer) {
      return user;
    }

    // If the user does not exist yet, create a new user record
    if (!user) {
      const newUser = await this.prisma.user.create({
        data: validateInfo
      });
      return newUser;
    }

    return null;
  }

  async isSignatureValid (message, address, signature) {
    try {
      const signerAddr = await ethers.verifyMessage(message, signature);
      if (signerAddr !== address) {
        return false;
      }
  
      return true;
    } catch (err) {
      console.log(err);
      return false;
    }
  };

  async login(user: User, session: Record<string, any>) {
    // Set user information in the session
    session.userId = user.id;
  }
}
