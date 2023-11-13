import { BadRequestException, ForbiddenException, Injectable, InternalServerErrorException } from '@nestjs/common';
import { UserEntity } from '../user/entities/user.entity'; // import your User entity
import { UserService } from '../user/user.service';
import { ethers } from 'ethers';
import { loginDto } from './dto/login.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { LOGIN_MESSAGE } from 'src/constants/web3Const/messages';
import { JwtService } from '@nestjs/jwt';
import { ConfigService } from '@nestjs/config';
import SecureCommon from 'src/commons/Secure.common';
import { User } from '@prisma/client';
@Injectable()
export class AuthService {
  constructor(private readonly configService: ConfigService, private userService: UserService, private readonly prisma: PrismaService, private jwtService: JwtService) {}

  async validateUser(validateInfo: loginDto): Promise<UserEntity> {
    const loginMessage = LOGIN_MESSAGE(validateInfo.date)
    const isSignatureValid = await this.isSignatureValid(loginMessage, validateInfo.signature, validateInfo.publicKey);
    if (!isSignatureValid) {
      throw new BadRequestException('Signature invalid');
    }

    const user = await this.userService.findByPublicKey(validateInfo.publicKey);
    if (user) {
      return user;
    }

    // If the user does not exist yet, create a new user record
    if (!user) {
      const data = { ...validateInfo, signDate: new Date(validateInfo.date), signedMessage: loginMessage };
      delete data.date;
      const newUser = await this.prisma.user.create({
        data
      });
      return newUser;
    }
  }

  async refreshTokens(refreshToken: string) {
    const userId = await SecureCommon.getSessionInfo(refreshToken);
    if (!userId) {
      throw new Error('Token not found');
    }
    const user = await this.prisma.user.findUnique({
      where: {
        id: parseInt(userId),
      },
    });
    if (!user) {
      throw new ForbiddenException('Access Denied');
    }
    const tokens = await this.getTokens(
      user.signer,
      user.id.toString(),
    );
    await this.updateRefreshTokenCaching(user, tokens.refreshToken);
    await this.updateRefreshTokenCaching(user, refreshToken, true);
    return tokens;
  }

  async updateRefreshTokenCaching(user: UserEntity, token: string, isLogout = false) {
    if (!isLogout) {
      SecureCommon.storeSession(user, token);
    } else {
      SecureCommon.deleteSessionInfo(token);
    }
  }
  
  async getTokens(signer: string, userId: string) {
    const [accessToken, refreshToken] = await Promise.all([
      this.jwtService.signAsync(
        {
          sub: userId,
          payload: signer,
        },
        {
          secret: this.configService.get<string>('JWT_SECRET'),
          expiresIn: '120m',
        },
      ),
      this.jwtService.signAsync(
        {
          sub: userId,
          payload: signer,
        },
        {
          secret: this.configService.get<string>('JWT_SECRET'),
          expiresIn: '7d',
        },
      ),
    ]);
    const refreshTokenExpire = new Date().setDate(new Date().getDate() + 7);
    const accessTokenExpire = new Date().setTime(
      new Date().getTime() + 5000 * 60,
    );
    return {
      accessToken,
      refreshToken,
      refreshTokenExpire,
      accessTokenExpire,
      userId,
    };
  }

  async isSignatureValid (message, signature, address) {
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

  async login(user: UserEntity) {
    // Set user information in the session
    const { accessToken, refreshToken, accessTokenExpire, refreshTokenExpire, userId } =
      await this.getTokens(user.signer, user.id.toString());
    await this.updateRefreshTokenCaching(user, refreshToken, false);
    if (!accessToken) 
      throw new InternalServerErrorException();
    return {
      refreshToken,
      refreshTokenExpire,
      accessToken,
      accessTokenExpire,
      userId,
    }
  }
}
