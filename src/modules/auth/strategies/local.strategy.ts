import { Injectable, UnauthorizedException } from '@nestjs/common';
import { PassportStrategy } from '@nestjs/passport';
import { Strategy } from 'passport-local';
import { AuthService } from '../auth.service';
import { loginDto } from '../dto/login.dto';

@Injectable()
export class LocalStrategy extends PassportStrategy(Strategy) {
  constructor(private readonly authService: AuthService) {
    super();
  }

  async validate(loginInfo: loginDto): Promise<any> {
    const user = await this.authService.validateUser(loginInfo);
    if (!user) {
      throw new UnauthorizedException();
    }
    return user;
  }
}
