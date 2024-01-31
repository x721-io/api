import {
  Injectable,
  UnauthorizedException,
  ExecutionContext,
} from '@nestjs/common';
import { AuthGuard } from '@nestjs/passport';

@Injectable()
export class AuthenticationCustomizeGuard extends AuthGuard('jwt') {
  handleRequest(err: any, user: any, info: any, context: ExecutionContext) {
    // If there's an error (including expired token), throw UnauthorizedException
    if (err || (info && info.name === 'TokenExpiredError')) {
      throw new UnauthorizedException();
    }
    // If there's no user (token not present or invalid), set user to null
    if (!user) {
      context.switchToHttp().getRequest().user = null;
    }

    return user;
  }
}
