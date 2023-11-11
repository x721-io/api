import { HttpException, HttpStatus } from '@nestjs/common';

export class SessionExpiredException extends HttpException {
  constructor(msg: string) {
    super(msg, HttpStatus.UNAUTHORIZED);
  }
}
