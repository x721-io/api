import {
  Controller,
  Get,
  Post,
  Body,
  Patch,
  Param,
  Delete,
  UseGuards,
  Put,
  Query,
} from '@nestjs/common';
import { PlatformOverviewService } from './platform-overview.service';
import {
  CreatePlatformOverviewDto,
  PlatformOverviewFilter,
} from './dto/create-platform-overview.dto';
import { UpdatePlatformOverviewDto } from './dto/update-platform-overview.dto';
import { AuthenticationGuard } from '../auth/guards/auth.guard';
import { GetCurrentUser } from '../../decorators/get-current-user.decorator';
import { User } from '@prisma/client';

@Controller('platform-overview')
export class PlatformOverviewController {
  constructor(
    private readonly platformOverviewService: PlatformOverviewService,
  ) {}

  @Post()
  @UseGuards(AuthenticationGuard)
  create(
    @Body() createPlatformOverviewDto: CreatePlatformOverviewDto,
    @GetCurrentUser() user: User,
  ) {
    return this.platformOverviewService.create(createPlatformOverviewDto, user);
  }

  @Get()
  findAll(@Query() filter: PlatformOverviewFilter) {
    return this.platformOverviewService.findAll(filter);
  }

  @Get(':id')
  findOne(@Param('id') id: string, @Query() filter: PlatformOverviewFilter) {
    return this.platformOverviewService.findOne(id, filter);
  }

  @Put(':id')
  @UseGuards(AuthenticationGuard)
  update(
    @Param('id') id: string,
    @Body() updatePlatformOverviewDto: UpdatePlatformOverviewDto,
    @GetCurrentUser() user: User,
  ) {
    return this.platformOverviewService.update(
      id,
      updatePlatformOverviewDto,
      user,
    );
  }

  @Delete(':id')
  @UseGuards(AuthenticationGuard)
  remove(@Param('id') id: string, @GetCurrentUser() user: User) {
    return this.platformOverviewService.remove(id, user);
  }
}
