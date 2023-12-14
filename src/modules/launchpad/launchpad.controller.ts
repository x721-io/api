import {
  Controller,
  Get,
  Post,
  Body,
  Patch,
  Param,
  Delete,
  UseGuards,
  Query,
} from '@nestjs/common';
import { LaunchpadService } from './launchpad.service';
import { CreateLaunchpadDto } from './dto/create-launchpad.dto';
import { UpdateLaunchpadDto } from './dto/update-launchpad.dto';
import { CheckStakingDto } from './dto/check-staking.dto';
import { AuthenticationGuard } from '../auth/guards/auth.guard';
import { User } from '@prisma/client';
import { GetCurrentUser } from 'src/decorators/get-current-user.decorator';
import { FindAllProjectDto } from './dto/find-all-project.dto';

@Controller('launchpad')
export class LaunchpadController {
  constructor(private readonly launchpadService: LaunchpadService) {}

  // @Post()
  // create(@Body() createLaunchpadDto: CreateLaunchpadDto) {
  //   return this.launchpadService.create(createLaunchpadDto);
  // }

  @Post('/job-config-round/:id')
  activateProject(@Param('id') id: string) {
    return this.launchpadService.configNextRound(id);
  }

  @Get()
  findAll(@Query() query: FindAllProjectDto) {
    return this.launchpadService.findAll(query);
  }

  @Get(':id')
  findOne(@Param('id') id: string) {
    return this.launchpadService.findOne(id);
  }

  @Post()
  @UseGuards(AuthenticationGuard)
  async checkStak(
    @Body() inputStaking: CheckStakingDto,
    @GetCurrentUser() user: User,
  ) {
    return await this.launchpadService.checkStaking(inputStaking, user);
  }
}
