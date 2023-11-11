import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { logger } from 'src/commons';

@Injectable()
export class WebhookService {
  constructor(
    private readonly prisma: PrismaService,
  ) {}

}
