import { Controller, Get, Post, Body, Patch, Param, Delete, UseGuards } from '@nestjs/common';
import { UserService } from './user.service';
import { CreateUserDto } from './dto/create-user.dto';
import { UpdateUserDto } from './dto/update-user.dto';
import { loginDto } from '../auth/dto/login.dto';
import { AuthenticatedGuard } from '../auth/guards/authenticated.guard';
import { AuthGuard } from '@nestjs/passport';
import { GetCurrentUser } from 'src/decorators/get-current-user.decorator';
import { User } from '@prisma/client';
import { AuthenticationGuard } from '../auth/guards/auth.guard';

@Controller('user')
export class UserController {
  constructor(private readonly userService: UserService) {}

  @Post('profile')
  @UseGuards(AuthenticationGuard)
  async updateProfile(@Body() updateProfile: UpdateUserDto, @GetCurrentUser() user: User) {
    console.log(user);
    return await this.userService.updateProfile(updateProfile, user);
  }
}
