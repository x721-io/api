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
  // Seed User
  // const creator = await prisma.user.upsert({
  //   where: { signer: '0x0d3c3d95df3c9e71d39fd00eb842026713ad64fe' },
  //   update: {
  //     username: 'X721',
  //     email: "X721@gmail.com",
  //   },
  //   create: {
  //     id: '43dbe7f6-273e-4a0e-89c8-6a819b5e7f18',
  //     username: 'X721',
  //     signer: '0x0d3c3d95df3c9e71d39fd00eb842026713ad64fe',
  //     publicKey: '0x0d3C3d95dF3c9e71d39fd00Eb842026713ad64fE',
  //     accountStatus: true,
  //     updatedAt: new Date('2024-10-25T06:54:22.253Z'),
  //   },
  // });

  // // Seed Collections
  // const ERC721Base = await prisma.collection.upsert({
  //   where: { address: '0x173bc76e109697d1c65d864072847f0cf3743b82' },
  //   update: {},
  //   create: {
  //     id: 'c3a0c165-a179-4135-a53c-42d577d9e9a9',
  //     txCreationHash: '0x5f80d996cb00170e77c87a45c7930703a614004d3e4c7f10c964ce0d8ba76056',
  //     name: 'ERC721Base',
  //     symbol: 'ERC721Base',
  //     status: 'SUCCESS',
  //     type: 'ERC721',
  //     address: '0x173bc76e109697d1c65d864072847f0cf3743b82',
  //     updatedAt: new Date('2024-10-25T06:54:22.253Z'),
  //   },
  // });

  // const ERC1155Base = await prisma.collection.upsert({
  //   where: { address: '0x68f903865e703da4de7804d2973182896efe073c' },
  //   update: {},
  //   create: {
  //     id: 'c3a0c165-a179-4135-a53c-42d577d9e9a9',
  //     txCreationHash: '0xf72eb7d81a9788345bff7b6793b1f8ad278b8cbbf89a5083b77c621d3519f140',
  //     name: 'ERC1155Base',
  //     symbol: 'ERC1155Base',
  //     status: 'SUCCESS',
  //     type: 'ERC1155',
  //     address: '0x68f903865e703da4de7804d2973182896efe073c',
  //     updatedAt: new Date('2024-10-25T06:54:22.253Z'),
  //   },
  // });

  // await prisma.userCollection.upsert({
  //   where: {
  //     userId_collectionId: {
  //       userId: creator.id,
  //       collectionId: ERC721Base.id,
  //     }
  //   },
  //   create: {
  //     userId: creator.id,
  //     collectionId: ERC721Base.id,
  //   },
  //   update: {}
  // })

  // await prisma.userCollection.upsert({
  //   where: {
  //     userId_collectionId: {
  //       userId: creator.id,
  //       collectionId: ERC1155Base.id,
  //     }
  //   },
  //   create: {
  //     userId: creator.id,
  //     collectionId: ERC1155Base.id,
  //   },
  //   update: {}
  // })
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
