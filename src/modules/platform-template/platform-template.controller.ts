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
} from '@nestjs/common';
import { PlatformTemplateService } from './platform-template.service';
import { CreatePlatformTemplateDto } from './dto/create-platform-template.dto';
import { UpdatePlatformTemplateDto } from './dto/update-platform-template.dto';
import { AuthenticationGuard } from '../auth/guards/auth.guard';
import { GetCurrentUser } from '../../decorators/get-current-user.decorator';
import { User } from '@prisma/client';

@Controller('platform-template')
export class PlatformTemplateController {
  constructor(
    private readonly platformTemplateService: PlatformTemplateService,
  ) {}

  @Post()
  @UseGuards(AuthenticationGuard)
  create(
    @Body() createPlatformTemplateDto: CreatePlatformTemplateDto,
    @GetCurrentUser() user: User,
  ) {
    return this.platformTemplateService.create(createPlatformTemplateDto, user);
  }

  @Get(':id')
  findOne(@Param('id') id: string) {
    return this.platformTemplateService.findOne(id);
  }

  @Put(':id')
  @UseGuards(AuthenticationGuard)
  update(
    @Param('id') id: string,
    @Body() updatePlatformTemplateDto: UpdatePlatformTemplateDto,
    @GetCurrentUser() user: User,
  ) {
    return this.platformTemplateService.update(
      id,
      updatePlatformTemplateDto,
      user,
    );
  }

  @Delete(':id')
  @UseGuards(AuthenticationGuard)
  remove(@Param('id') id: string, @GetCurrentUser() user: User) {
    return this.platformTemplateService.remove(id, user);
  }
}
