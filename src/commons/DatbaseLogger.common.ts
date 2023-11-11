import { PrismaClient } from '@prisma/client';
import { logger } from './';
import { Sql } from '@prisma/client/runtime';

const prisma = new PrismaClient();

class DatabaseLogger {
  async logQuery(query: string, parameters?: unknown[]) {
    await prisma.$queryRaw`${query}${parameters}`;
  }

  async logQueryError(error: string, query: string, parameters?: unknown[]) {
    await prisma.$queryRaw`${query}${parameters}`;
    logger.error(
      `${query} -- Parameters: ${this.stringifyParameters(
        parameters,
      )} -- ${error}`,
    );
  }

  logQuerySlow(time: number, query: string, parameters?: unknown[]) {
    logger.warn(
      `Time: ${time} -- Parameters: ${this.stringifyParameters(
        parameters,
      )} -- ${query}`,
    );
  }

  logMigration(message: string) {
    logger.info(message);
  }

  logSchemaBuild(message: string) {
    logger.info(message);
  }

  log(level: 'log' | 'info' | 'warn', message: string) {
    if (level === 'log' || level === 'info') {
      logger.info(message);
    } else if (level === 'warn') {
      logger.warn(message);
    }
  }

  private stringifyParameters(parameters?: unknown[]) {
    try {
      return JSON.stringify(parameters);
    } catch {
      return '';
    }
  }
}

export default DatabaseLogger;
