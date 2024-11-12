import { PrismaClient } from '@prisma/client';

const prisma = new PrismaClient();

async function main() {

//   Seed User
  const creator = await prisma.user.upsert({
    where: { signer: '0x0d3c3d95df3c9e71d39fd00eb842026713ad64fe' },
    update: {
      username: 'X721',
      email: "X721@gmail.com",
    },
    create: {
      id: '43dbe7f6-273e-4a0e-89c8-6a819b5e7f18',
      username: 'X721',
      signer: '0x0d3c3d95df3c9e71d39fd00eb842026713ad64fe',
      publicKey: '0x0d3C3d95dF3c9e71d39fd00Eb842026713ad64fE',
      accountStatus: true,
      updatedAt: new Date('2024-10-25T06:54:22.253Z'),
    },
  });

  // Seed Collections
  const ERC721Base = await prisma.collection.upsert({
    where: { address: '0x24ac3a41a0c67970ed16bd79a64d08a7f4f4e352' },
    update: {},
    create: {
      id: 'c3a0c165-a179-4135-a53c-42d577d9e9a9',
      txCreationHash: '0x9a122fde85dd0f64394acc1f03438dadbebfdeeaf12d75b6b11054ffc67d4a30',
      name: 'ERC721Base',
      symbol: 'ERC721Base',
      status: 'SUCCESS',
      type: 'ERC721',
      address: '0x24ac3a41a0c67970ed16bd79a64d08a7f4f4e352',
      updatedAt: new Date('2024-10-25T06:54:22.253Z'),
    },
  });

  const ERC1155Base = await prisma.collection.upsert({
    where: { address: '0xccf9fa2245394aca5c96038e4b8e493d11255f1b' },
    update: {},
    create: {
      id: '27c8db7f-c5b4-45fc-a42b-fee6011011c6',
      txCreationHash: '0x2ee649af002fad3fc71a0b9da5a97f5e23933dfc32bc7f3e7d7881a7b9bb45e9',
      name: 'ERC1155Base',
      symbol: 'ERC1155Base',
      status: 'SUCCESS',
      type: 'ERC1155',
      address: '0xccf9fa2245394aca5c96038e4b8e493d11255f1b',
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
