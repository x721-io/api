import { Controller, Get, Post, Body, Patch, Param, Delete, Session } from '@nestjs/common';
import { AuthService } from './auth.service';
import { CreateAuthDto } from './dto/create-auth.dto';
import { UpdateAuthDto } from './dto/update-auth.dto';
import { loginDto } from './dto/login.dto';

@Controller('auth')
export class AuthController {
  constructor(private readonly authService: AuthService) {}

  @Post('connect')
  async login(@Body() body: loginDto, @Session() session: Record<string, any>) {
    // body should contain the publicKey and signature
    const user = await this.authService.validateUser(body);
    if (user) {
      await this.authService.login(user, session);
      return { message: 'Logged in successfully' };
    } else {
      return { message: 'Authentication failed' };
    }
  }
}
