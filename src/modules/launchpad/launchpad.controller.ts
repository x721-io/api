import {
  Controller,
  Get,
  Post,
  Body,
  Patch,
  Param,
  Delete,
  Query,
} from '@nestjs/common';
import { LaunchpadService } from './launchpad.service';
import { CreateLaunchpadDto } from './dto/create-launchpad.dto';
import { UpdateLaunchpadDto } from './dto/update-launchpad.dto';
import { FindAllProjectDto } from './dto/find-all-project.dto';

@Controller('launchpad')
export class LaunchpadController {
  constructor(private readonly launchpadService: LaunchpadService) {}

  // @Post()
  // create(@Body() createLaunchpadDto: CreateLaunchpadDto) {
  //   return this.launchpadService.create(createLaunchpadDto);
  // }

  @Get()
  findAll(@Query() query: FindAllProjectDto) {
    return this.launchpadService.findAll(query);
  }

  @Get(':id')
  findOne(@Param('id') id: string) {
    return this.launchpadService.findOne(id);
  }
}
