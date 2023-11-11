import {
  Injectable,
  OnModuleInit,
  INestApplication,
  Logger,
} from '@nestjs/common';
import { Prisma, PrismaClient } from '@prisma/client';

@Injectable()
// 1.
export class PrismaService
  extends PrismaClient<Prisma.PrismaClientOptions, Prisma.LogLevel>
  implements OnModuleInit
{
  //2.
  private readonly logger = new Logger(PrismaService.name);

  constructor() {
    super({
      // 3.
      log: [
        {
          emit: 'event',
          level: 'query',
        },
        {
          emit: 'event',
          level: 'error',
        },
        {
          emit: 'event',
          level: 'info',
        },
        {
          emit: 'event',
          level: 'warn',
        },
      ],
    });
  }

  async onModuleInit() {
    //4.
    this.$on('error', (event) => {
      this.logger.error(event);
    });
    this.$on('warn', (event) => {
      this.logger.warn(event);
    });
    // this.$on('info', (event) => {
    //   this.logger.verbose(event);
    // });
    this.$on('query', (event) => {
      this.logger.log(event);
    });
    this.$use(async (params, next) => {
      const before = Date.now();
      const result = await next(params);
      const after = Date.now();

      this.logger.log(`Query took ${after - before}ms`);
      return result;
    });
    await this.$connect();
  }

  async enableShutdownHooks(app: INestApplication) {
    this.$on('beforeExit', async () => {
      await app.close();
    });
  }
}
