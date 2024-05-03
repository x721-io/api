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
import { TopicService } from '../service/topic-cms.service';
import { createOrUpdateTopicDto, getAllTopicDto } from '../dto/topic.dto';

@Controller('/cms/topic')
export class TopicController {
  constructor(private readonly topicService: TopicService) {}

  /** Topic */
  @Get()
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_BLOG, Role.CREATOR, Role.VIEWER)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async getListTopic(@Query() input: getAllTopicDto) {
    return this.topicService.getAllTopic(input);
  }

  @Post()
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_BLOG, Role.CREATOR, Role.VIEWER)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async createTopic(
    @Body() input: createOrUpdateTopicDto,
    @GetCurrentUser() account: Account,
  ) {
    return this.topicService.createTopic(input, account);
  }

  @Put()
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_BLOG, Role.CREATOR, Role.VIEWER)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async updateTopic(
    @Body() input: createOrUpdateTopicDto,
    @GetCurrentUser() account: Account,
  ) {
    return this.topicService.UpdateTopic(input, account);
  }

  @Delete('/:id')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_BLOG, Role.CREATOR, Role.VIEWER)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async deleteTopic(
    @Param('id') id: string,
    @GetCurrentUser() account: Account,
  ) {
    return this.topicService.deleteTopic(id, account);
  }

  @Get('/:id')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_BLOG, Role.CREATOR, Role.VIEWER)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async getDetailTopic(@Param('id') id: string) {
    return this.topicService.getDetailTopic(id);
  }
  /** End Topic */
}
