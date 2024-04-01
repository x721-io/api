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
import { LaunchPadService } from '../service/launchpad-cms.service';
import {
  CreateOrUpdateProjectDto,
  CreateRoundInforDto,
  FindAllProjectDto,
  GetAllRoundDto,
  UpdateRoundInforDto,
} from '../dto/launchpad.dto';

@Controller('/cms/launchpad')
export class LaunchPadController {
  constructor(private readonly launchPadService: LaunchPadService) {}
  /** CRUD Round */
  @Get('/search-round')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_LAUNCHPAD)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async findAllRound(@Query() input: GetAllRoundDto) {
    return this.launchPadService.findAllRound(input);
  }

  @Post('/create-round')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_LAUNCHPAD)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async createRoundInfor(
    @Body() input: CreateRoundInforDto,
    @GetCurrentUser() account: Account,
  ) {
    return this.launchPadService.createRoundInfor(input, account);
  }

  @Put('/update-round')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_LAUNCHPAD)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async UpdateRoundInfor(
    @Body() input: UpdateRoundInforDto,
    @GetCurrentUser() account: Account,
  ) {
    return this.launchPadService.updateRoundInfor(input, account);
  }

  @Delete('/round/:id')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_LAUNCHPAD)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async deleteRound(
    @Param('id') id: string,
    @GetCurrentUser() account: Account,
  ) {
    return this.launchPadService.deleteRound(id, account);
  }
  /** End Round */

  /** CRUD Project */
  @Get('/search-project')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_LAUNCHPAD)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async findAllProject(@Query() input: FindAllProjectDto) {
    return this.launchPadService.findAllProject(input);
  }

  @Post('/create-project')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_LAUNCHPAD)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async createProject(
    @Body() input: CreateOrUpdateProjectDto,
    @GetCurrentUser() account: Account,
  ) {
    return this.launchPadService.handleCreateProject(input, account);
  }

  @Put('/update-project')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_LAUNCHPAD)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async updateProject(
    @Body() input: CreateOrUpdateProjectDto,
    @GetCurrentUser() account: Account,
  ) {
    return this.launchPadService.handleUpdateProject(input, account);
  }

  @Delete('/project/:id')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_LAUNCHPAD)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async deleteProject(
    @Param('id') id: string,
    @GetCurrentUser() account: Account,
  ) {
    return this.launchPadService.handleDeleteProject(id, account);
  }

  @Get('/project/:id')
  @Roles(Role.ADMINISTRATOR, Role.ADMIN_LAUNCHPAD)
  @UseGuards(AuthRoleGuard, RoleGuard)
  async getDetailProject(@Param('id') id: string) {
    return this.launchPadService.getDetailProject(id);
  }
  // getDetailProject
}
