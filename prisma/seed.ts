import { PrismaClient } from '@prisma/client';

const prisma = new PrismaClient();

async function main() {
  await prisma.syncMasterData.update({
    where: {
      type: 'ERC721'
    },
    data: {
      timestamp: 0
    }
  })

  await prisma.syncMasterData.update({
    where: {
      type: 'ERC1155'
    },
    data: {
      timestamp: 0
    }
  })
  console.log('Seeding completed successfully.');
}

main()
  .catch((e) => {
    console.error(e);
    process.exit(1);
  })
  .finally(async () => {
    await prisma.$disconnect();
  });
