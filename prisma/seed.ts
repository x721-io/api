import { PrismaClient } from '@prisma/client';

const prisma = new PrismaClient();

async function main() {
  // Seed User
  const creator = await prisma.user.upsert({
    where: { signer: '0x2d16d2fc0074fd4c1442258dc6fcba8834a405c5' },
    update: {},
    create: {
      id: '6c811c32-39c9-4303-883b-9e9ff5882ed2',
      username: 'X721 Creator',
      signer: '0x2d16d2fc0074fd4c1442258dc6fcba8834a405c5',
      publicKey: '0x2d16D2fc0074FD4c1442258DC6fCBa8834a405C5',
      updatedAt: new Date('2024-10-25T06:54:22.253Z'),
    },
  });

  // Seed Collections
  const ERC721Base = await prisma.collection.upsert({
    where: { address: '0x7ddb1accb3160cf6ba4fee23e26b6d9ad45bc824' },
    update: {},
    create: {
      id: '5a799476-3129-4066-bd2f-30d2d4f48b3e',
      txCreationHash: '0x195d3eadeb3eb9837fb4d64cb2eccd2dd66b174b10f760508c9998cf23ae1d20',
      name: 'ERC721Base',
      symbol: 'ERC721Base',
      status: 'SUCCESS',
      type: 'ERC721',
      address: '0x7ddb1accb3160cf6ba4fee23e26b6d9ad45bc824',
      updatedAt: new Date('2024-10-25T06:54:22.253Z'),
    },
  });

  const ERC1155Base = await prisma.collection.upsert({
    where: { address: '0xda4a022bbc044b8097159c8f4527fe7c6111a70c' },
    update: {},
    create: {
      id: '849003e2-5010-46d2-bb02-9bdc0dbd1384',
      txCreationHash: '0xb4bda9104f049a6b0383981c06665ca40da15cb95a09b08f916969b6802ec1ea',
      name: 'ERC1155Base',
      symbol: 'ERC1155Base',
      status: 'SUCCESS',
      type: 'ERC1155',
      address: '0xda4a022bbc044b8097159c8f4527fe7c6111a70c',
      updatedAt: new Date('2024-10-25T06:54:22.253Z'),
    },
  });

  await prisma.userCollection.upsert({
    where: {
      userId_collectionId: {
        userId: creator.id,
        collectionId: ERC721Base.id,
      }
    },
    create: {
      userId: creator.id,
      collectionId: ERC721Base.id,
    },
    update: {}
  })

  await prisma.userCollection.upsert({
    where: {
      userId_collectionId: {
        userId: creator.id,
        collectionId: ERC1155Base.id,
      }
    },
    create: {
      userId: creator.id,
      collectionId: ERC1155Base.id,
    },
    update: {}
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
