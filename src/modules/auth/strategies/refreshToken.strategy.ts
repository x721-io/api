import { PassportStrategy } from '@nestjs/passport';
// import { ExtractJwt, Strategy } from 'passport-jwt';
import { Strategy } from 'passport-local';
import { Inject, Injectable } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { UserService } from 'src/modules/user/user.service';

@Injectable()
export class RefreshTokenStrategy extends PassportStrategy(
  Strategy,
  'jwt-refresh',
) {
  constructor(
    @Inject(ConfigService) config: ConfigService,
    private usersService: UserService,
  ) {
    //     super({
    //   jwtFromRequest: ExtractJwt.fromAuthHeaderAsBearerToken(),
    //   secretOrKey: config.get('SECRET'),
    //   passReqToCallback: true,
    // });
    super();
  }

  validate(payload: any) {
    return { ...payload };
  }
}
