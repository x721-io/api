import { PrismaClient } from '@prisma/client';

const prisma = new PrismaClient();

export const HandleEntityManager =
  (index: number) => (target: any, key: string, descriptor: any) => {
    const originalMethod = descriptor.value;
    descriptor.value = async function (...args: any[]) {
      if (args.length < index) args[index - 1] = prisma;
      return await originalMethod.apply(this, args);
    };
    return descriptor;
  };
