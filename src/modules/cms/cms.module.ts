import { Module } from '@nestjs/common';
import { CMSService } from './service/cms.service';
import { CMSController } from './controller/cms.controller';
import { AuthCMSService } from './service/auth-cms.service';
import { AccessControlService } from '../../shared/AccessControlService';
import { PrismaService } from 'src/prisma/prisma.service';
import { JwtModule } from '@nestjs/jwt';
import { ConfigModule, ConfigService } from '@nestjs/config';
import { PassportModule } from '@nestjs/passport';
import { JwtStrategy } from '../auth/strategies/jwt.strategy';
import { RefreshTokenStrategy } from '../auth/strategies/refreshToken.strategy';
import { LocalStrategy } from '../auth/strategies/local.strategy';
import { UserService } from '../user/user.service';
import { AuthService } from '../auth/auth.service';
import { ActivityService } from '../nft/activity.service';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';
import { NftService } from '../nft/nft.service';
import { TokenService } from '../nft/token.service';
import { MarketplaceService } from '../nft/nft-marketplace.service';
import { ValidatorService } from '../validator/validator.service';
import { CollectionPriceService } from '../collection/collectionPrice.service';
import { GetCollectionMarketData } from '../graph-qlcaller/getCollectionMarketData.service';
import { TraitService } from '../nft/trait.service';
import { CollectionModule } from '../collection/collection.module';
import { MarketplaceCMSService } from './service/marketplace-cms.service';
import { LaunchPadService } from './service/launchpad-cms.service';
import { CMSOptionService } from './service/option-cms.service';
import { TopicService } from './service/topic-cms.service';
import { BlogService } from './service/blog-cms.service';
import { MarketplaceController } from './controller/marketplace-cms.controller';
import { LaunchPadController } from './controller/launchpad-cms.controller';
import { TopicController } from './controller/topic-cms.controller';
import { BlogController } from './controller/blog-cms.controller';
import { AccountController } from './controller/account-cms.controller';
import { NFTHepler } from '../nft/helper/nft-helper.service';
@Module({
  imports: [
    JwtModule.registerAsync({
      imports: [ConfigModule],
      inject: [ConfigService],
      useFactory: async (configService: ConfigService) => ({
        signOptions: {
          expiresIn: '1d',
        },
        secret: process.env.JWT_SECRET_ADMIN,
      }),
    }),
    ConfigModule,
    PassportModule,
    CollectionModule,
  ],
  controllers: [
    CMSController,
    MarketplaceController,
    LaunchPadController,
    TopicController,
    BlogController,
    AccountController,
  ],
  providers: [
    AuthService,
    UserService,
    GraphQlcallerService,
    ActivityService,
    PrismaService,
    JwtStrategy,
    RefreshTokenStrategy,
    LocalStrategy,
    AuthCMSService,
    AccessControlService,
    CMSService,
    NftService,
    TokenService,
    MarketplaceService,
    ValidatorService,
    CollectionPriceService,
    GetCollectionMarketData,
    TraitService,
    MarketplaceCMSService,
    LaunchPadService,
    CMSOptionService,
    TopicService,
    BlogService,
    NFTHepler,
  ],
})
export class CMSModule {}
