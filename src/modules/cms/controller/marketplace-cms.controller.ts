import { Controller, Get, Post, Body, UseGuards, Query } from '@nestjs/common';
import { CMSService } from '../service/cms.service';
import { Role } from '../../../constants/enums/role.enum';
import { Roles } from '../../../decorators/roles.decorator';
import { AuthRoleGuard } from '../../auth/guards/authRole.guard';
import { RoleGuard } from '../../auth/guards/role.guard';
import { AuthCMSService } from '../service/auth-cms.service';
import { GetAllAccountDto } from '../dto/get-all-account.dto';
import { GetAllNftDto } from '../../nft/dto/get-all-nft.dto';
import { GetAllCollectionDto } from '../../collection/dto/get-all-collection.dto';
import { GetAllUser } from '../../user/dto/get-all-user.dto';
import { MarketplaceCMSService } from '../service/marketplace-cms.service';
@Controller('cms/manage')
export class MarketplaceController {
  constructor(
    private readonly cmsService: CMSService,
    private readonly authCMSService: AuthCMSService,
    private readonly marketplaceCMSService: MarketplaceCMSService,
  ) {}
  @Post('/search-nft')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_NFT, Role.ADMIN_MARKETPLACE)
  @UseGuards(AuthRoleGuard, RoleGuard)
  findAllNFT(@Body() query: GetAllNftDto) {
    return this.marketplaceCMSService.findAllNFT(query);
  }

  @Get('/search-collection')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_COLLECTION, Role.ADMIN_MARKETPLACE)
  @UseGuards(AuthRoleGuard, RoleGuard)
  findAllCollection(@Query() input: GetAllCollectionDto) {
    return this.marketplaceCMSService.findAllCollection(input);
  }

  @Get('/search-user')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_USER, Role.ADMIN_MARKETPLACE)
  @UseGuards(AuthRoleGuard, RoleGuard)
  findAllUser(@Query() input: GetAllUser) {
    return this.marketplaceCMSService.findAllUser(input);
  }
  @Get('/search-account')
  @Roles(
    Role.ADMINISTRATOR,
    Role.ADMIN_COLLECTION,
    Role.ADMIN_BLOG,
    Role.CREATOR,
    Role.ADMIN_MARKETPLACE,
    Role.ADMIN_NFT,
    Role.ADMIN_LAUNCHPAD,
    Role.ADMIN_USER,
    Role.VIEWER,
  )
  @UseGuards(AuthRoleGuard, RoleGuard)
  findAll(@Query() query: GetAllAccountDto) {
    return this.cmsService.findAll(query);
  }
}
