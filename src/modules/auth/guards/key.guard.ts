import {
  Injectable,
  CanActivate,
  ExecutionContext,
  ForbiddenException,
} from '@nestjs/common';

@Injectable()
export class KeyGuard implements CanActivate {
  canActivate(context: ExecutionContext): boolean {
    const request = context.switchToHttp().getRequest();
    console.log(request.headers);
    const key = request.headers['webhook-token']; // Adjust based on where the key is located (headers, query, body, etc.)
    if (key === process.env.API_KEY_WEBHOOK) {
      return true; // Allow access
    } else {
      throw new ForbiddenException('Forbidden: Invalid token');
    }
  }
}
