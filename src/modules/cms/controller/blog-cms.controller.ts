import {
  Controller,
  Get,
  Post,
  Body,
  Param,
  Put,
  UseGuards,
  Query,
  Delete,
} from '@nestjs/common';
import { Role } from '../../../constants/enums/role.enum';
import { Roles } from '../../../decorators/roles.decorator';
import { AuthRoleGuard } from '../../auth/guards/authRole.guard';
import { RoleGuard } from '../../auth/guards/role.guard';
import { GetCurrentUser } from 'src/decorators/get-current-user.decorator';
import { Account } from '@prisma/client';
import { BlogService } from '../service/blog-cms.service';
import {
  activeBlogDto,
  createOrUpdateBlogDto,
  getAllBlog,
} from '../dto/blog.dto';

@Controller('/cms/blog')
export class BlogController {
  constructor(private readonly blogService: BlogService) {}
  @Post()
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_BLOG, Role.CREATOR)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async createBlog(
    @Body() input: createOrUpdateBlogDto,
    @GetCurrentUser() account: Account,
  ) {
    return this.blogService.createBlog(input, account);
  }

  @Get('/:id')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_BLOG, Role.CREATOR, Role.VIEWER)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async getDetailTopic(@Param('id') id: string) {
    return this.blogService.getDetail(id);
  }

  @Get()
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_BLOG, Role.CREATOR, Role.VIEWER)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async getListTopic(@Query() input: getAllBlog) {
    return this.blogService.getAllBlog(input);
  }

  @Put()
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_BLOG, Role.CREATOR)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async updateBlog(
    @Body() input: createOrUpdateBlogDto,
    @GetCurrentUser() account: Account,
  ) {
    return this.blogService.updateBlog(input, account);
  }

  @Delete('/:id')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_BLOG, Role.CREATOR)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async deleteBlog(
    @Param('id') id: string,
    @GetCurrentUser() account: Account,
  ) {
    return this.blogService.deleteBlog(id, account);
  }

  @Post('/active-blog')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_BLOG)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async activeBlog(
    @Body() input: activeBlogDto,
    @GetCurrentUser() account: Account,
  ) {
    return this.blogService.activeBlog(input, account);
  }
}
