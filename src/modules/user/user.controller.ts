import { Controller, Get, Post, Body, Patch, Param, Delete, UseGuards } from '@nestjs/common';
import { UserService } from './user.service';
import { CreateUserDto } from './dto/create-user.dto';
import { UpdateUserDto } from './dto/update-user.dto';
import { loginDto } from '../auth/dto/login.dto';
import { AuthenticatedGuard } from '../auth/guards/authenticated.guard';

@Controller('user')
export class UserController {
  constructor(private readonly userService: UserService) {}

  @Post('profile')
  @UseGuards(AuthenticatedGuard)
  async updateProfile(@Body() updateProfile: UpdateUserDto) {
    await this.userService.updateProfile(updateProfile);
  }
}
