// import { PrismaClient, ProjectRound } from '@prisma/client';

// const prisma = new PrismaClient();

// async function main() {

// // const ERC721BitGet = await prisma.collection.upsert({
// //     where: { address: '0x2ac49144a804e1f8652e1a0c1bad90606ac2094a' },
// //     update: {},
// //     create: {
// //         id: 'f35961f7-4a6a-4a39-b048-948e7636ccfd',
// //         txCreationHash: '0xa21ec8e8c105af74cc34a995372b134f84f0f010480b680fbe214462facc8786',
// //         name: 'U2U Network x Bitget Wallet',
// //         symbol: 'U2UxBGW',
// //         status: 'SUCCESS',
// //         type: 'ERC721',
// //         description : 'U2U Network x Bitget Wallet Incentivized Campaign',
// //         address: '0x2ac49144a804e1f8652e1a0c1bad90606ac2094a',
// //         subgraphUrl: 'https://graph-02.u2u.xyz/subgraphs/name/u2u/u2u-bitget',
// //         coverImage: 'https://ug-assets-dev.s3.ap-southeast-1.amazonaws.com/eafe210c-44f5-4f30-9a77-061e25783ff5-coverImage.png',
// //         avatar: 'https://ug-assets-dev.s3.ap-southeast-1.amazonaws.com/3820948b-8a64-4293-aad4-c147cfe6caff-BITGET%20X%20U2U%20Network%20NFT.png',
// //         updatedAt: new Date('2024-10-25T06:54:22.253Z'),
// //         flagExtend: true,
// //         },
// //     });

//     // const creator = await prisma.userCollection.create({
//     //     data: {
//     //         collectionId : 'f35961f7-4a6a-4a39-b048-948e7636ccfd',
//     //         userId: '43dbe7f6-273e-4a0e-89c8-6a819b5e7f18'
//     //     }
//     // })

//     // const roundInfo = await prisma.roundInfo.createMany({
//     //     data: [
//     //         {
//     //             name :'Whitelist Mint',
//     //             id: 2,
//     //             type : 'U2UMintRoundWhitelist',
//     //             isActive : true,
//     //             isDelete: false
//     //         },
//     //         {
//     //             name :'FCFS Mint',
//     //             id: 3,
//     //             type : 'U2UMintRoundFCFS',
//     //             isActive : true,
//     //             isDelete: false
//     //         },
//     //         {
//     //             name :'Round zero Premint',
//     //             id: 4,
//     //             type : 'U2UPremintRoundZero',
//     //             isActive : true,
//     //             isDelete: false
//     //         },
//     //         {
//     //             name :'Whitelist Premint',
//     //             id: 5,
//     //             type : 'U2UPremintRoundWhitelist',
//     //             isActive : true,
//     //             isDelete: false
//     //         },
//     //         {
//     //             name :'FCFS Premint',
//     //             id: 6,
//     //             type : 'U2UPremintRoundFCFS',
//     //             isActive : true,
//     //             isDelete: false
//     //         },
//     //         {
//     //             name :'Round zero Mint',
//     //             id: 1,
//     //             type : 'U2UMintRoundZero',
//     //             isActive : true,
//     //             isDelete: false
//     //         },
//     //         {
//     //             name :'Whitelist Mint',
//     //             id: 7,
//     //             type : 'U2UMintRoundWhitelistCustomized',
//     //             isActive : true,
//     //             isDelete: false
//     //         },
//     //         {
//     //             name :'Memetaverse Round',
//     //             id: 8,
//     //             type : 'Memetaverse',
//     //             isActive : true,
//     //             isDelete: false
//     //         }
            
//     //     ]
//     // }) 
//     // const ProjectRound = await prisma.projectRound.create({
//     //     data: {
//     //         projectId : "2d7e9dcb-aa1a-4c9e-ba95-b4a5b94d0536",
//     //         address: "0xcb56b723abb4202553e0facd48bcf18a6c6fbf21",
//     //         end: new Date("2025-02-01 5:00:00.000"),
//     //         start: new Date("2024-11-11 5:00:00.000"),
//     //         roundId: 3,
//     //         claimableStart: new Date("2024-11-11 5:00:00.000"),
//     //         maxPerWallet : 1,
//     //         price: "500000000000000000",
//     //         totalNftt: 0,
//     //         requiredStaking : "0",
//     //         stakeBefore : new Date("2024-11-11 5:00:00.000"),
//     //         instruction: "https://x721.io/"
//     //     }
//     // })

//     // const updateProjectCollection = await prisma.collection.update({
//     //     data: {
//     //         isVerified: true
//     //     },
//     //     where: {
//     //         id : "f35961f7-4a6a-4a39-b048-948e7636ccfd"
//     //     }
//     // })

//     // const updateNFT = await prisma.nFT.updateMany({
//     //     data: {
//     //         image : "https://indigo-accessible-raccoon-107.mypinata.cloud/ipfs/QmXahNKuC1ji1j9qCoEaopp8DVooWeUAnwBYSV1GpQtSb5",
//     //         tokenUri : "https://indigo-accessible-raccoon-107.mypinata.cloud/ipfs/QmTjTtDbkhgDHQ7MVvpZnFWNwX1Lm8MsMis9CPjGeSr1Wz"
//     //     },
//     //     where: {
//     //         collectionId : "f35961f7-4a6a-4a39-b048-948e7636ccfd"
//     //     }
//     // })

//     // const updateProject = await prisma.project.update({
//     //   data: {
//     //     banner: "https://ug-assets-dev.s3.ap-southeast-1.amazonaws.com/9e9db6ba-6d04-4507-a3e2-a3b7b8ec3141-bitgetWallet.png",
//     //     logo: "https://ug-assets-dev.s3.ap-southeast-1.amazonaws.com/9e9db6ba-6d04-4507-a3e2-a3b7b8ec3141-bitgetWallet.png",
//     //     organization : "U2U Network x Bitget Wallet"
//     //   },
//     //   where: {
//     //     id : "2d7e9dcb-aa1a-4c9e-ba95-b4a5b94d0536"
//     //   }
//     // })

//     // const updateCollection = await prisma.collection.update({
//     //   data: {
//     //     shortUrl: "u2uxbitgetwallet"
//     //   },
//     //   where: {
//     //     id : "f35961f7-4a6a-4a39-b048-948e7636ccfd"
//     //   }
//     // })
//     // console.log("🚀 ~ main ~ updateNFT:", updateNFT)
//     // const updateProjectRound = await prisma.projectRound.update({
//     //     data: {
//     //         // address : "0xd987584bda5e1bf12fc5b64dbfe5060c60bc0738",
//     //         // price: "0"
//     //         end: new Date("2025-2-8 5:00:00.000"),
//     //     },
//     //     where: {
//     //         projectId_roundId:{
//     //             projectId : "2d7e9dcb-aa1a-4c9e-ba95-b4a5b94d0536",
//     //             roundId : 3,
//     //         }
//     //     }
//     // })
//     // console.log("🚀 ~ main ~ updateProjectRound:", updateProjectRound)

//     // const updateCollection = await prisma.collection.update({
//     //     data: {
//     //         isU2U: false
//     //     },
//     //     where: {
//     //         id : "f35961f7-4a6a-4a39-b048-948e7636ccfd"
//     //     }
//     // })

//     // const updatecreator = await prisma.user.update({
//     //   data: {
//     //     accountStatus: true,
//     //   },
//     //   where: {
//     //     id : "43dbe7f6-273e-4a0e-89c8-6a819b5e7f18"
//     //   }
//     // })

//     // console.log("🚀 ~ main ~ updateProjectCollection:", updateProjectCollection)


//     // const updateSet1155 = await prisma.syncMasterData.update({
//     //   data: {
//     //     timestamp : 0
//     //   },
//     //   where: {
//     //     type: 'ERC1155'
//     //   }
//     // })

//     // const updateSet721 = await prisma.syncMasterData.update({
//     //   data: {
//     //     timestamp : 0
//     //   },
//     //   where: {
//     //     type: 'ERC721'
//     //   }
//     // })
//     // console.log("🚀 ~ main ~ updateSet721:", updateSet721)
//     // console.log("🚀 ~ main ~ updateSet1155:", updateSet1155)
//     // // const select = await prisma.marketplaceStatus.findMany();
//     // // const select1 = await prisma.syncMasterData.findMany();
//     // console.log("🚀 ~ main ~ updateCollection:", updateCollection)

//     // https://indigo-accessible-raccoon-107.mypinata.cloud/ipfs/QmP5rVzqDnHsNdSF611HBL59fxuzdXEFZ3dWfV5rxvjRuJ
//     // console.log("🚀 ~ main ~ updateProjectCollection:", updateProjectCollection)
//     // console.log("🚀 ~ main ~ ProjectRound:", ProjectRound)
//     // console.log("🚀 ~ main ~ ERC721BitGet:", ERC721BitGet)
//     // console.log("🚀 ~ main ~ creator:", creator)
// // //   Seed User
// //   const creator = await prisma.user.upsert({
// //     where: { signer: '0x0d3c3d95df3c9e71d39fd00eb842026713ad64fe' },
// //     update: {
// //       username: 'X721',
// //       email: "X721@gmail.com",
// //     },
// //     create: {
// //       id: '43dbe7f6-273e-4a0e-89c8-6a819b5e7f18',
// //       username: 'X721',
// //       signer: '0x0d3c3d95df3c9e71d39fd00eb842026713ad64fe',
// //       publicKey: '0x0d3C3d95dF3c9e71d39fd00Eb842026713ad64fE',
// //       accountStatus: true,
// //       updatedAt: new Date('2024-10-25T06:54:22.253Z'),
// //     },
// //   });

// //   // Seed Collections
// //   const ERC721Base = await prisma.collection.upsert({
// //     where: { address: '0x24ac3a41a0c67970ed16bd79a64d08a7f4f4e352' },
// //     update: {},
// //     create: {
// //       id: 'c3a0c165-a179-4135-a53c-42d577d9e9a9',
// //       txCreationHash: '0x9a122fde85dd0f64394acc1f03438dadbebfdeeaf12d75b6b11054ffc67d4a30',
// //       name: 'ERC721Base',
// //       symbol: 'ERC721Base',
// //       status: 'SUCCESS',
// //       type: 'ERC721',
// //       address: '0x24ac3a41a0c67970ed16bd79a64d08a7f4f4e352',
// //       updatedAt: new Date('2024-10-25T06:54:22.253Z'),
// //     },
// //   });

// //   const ERC1155Base = await prisma.collection.upsert({
// //     where: { address: '0xccf9fa2245394aca5c96038e4b8e493d11255f1b' },
// //     update: {},
// //     create: {
// //       id: '27c8db7f-c5b4-45fc-a42b-fee6011011c6',
// //       txCreationHash: '0x2ee649af002fad3fc71a0b9da5a97f5e23933dfc32bc7f3e7d7881a7b9bb45e9',
// //       name: 'ERC1155Base',
// //       symbol: 'ERC1155Base',
// //       status: 'SUCCESS',
// //       type: 'ERC1155',
// //       address: '0xccf9fa2245394aca5c96038e4b8e493d11255f1b',
// //       updatedAt: new Date('2024-10-25T06:54:22.253Z'),
// //     },
// //   });

// //   await prisma.userCollection.upsert({
// //     where: {
// //       userId_collectionId: {
// //         userId: creator.id,
// //         collectionId: ERC721Base.id,
// //       }
// //     },
// //     create: {
// //       userId: creator.id,
// //       collectionId: ERC721Base.id,
// //     },
// //     update: {}
// //   })

// //   await prisma.userCollection.upsert({
// //     where: {
// //       userId_collectionId: {
// //         userId: creator.id,
// //         collectionId: ERC1155Base.id,
// //       }
// //     },
// //     create: {
// //       userId: creator.id,
// //       collectionId: ERC1155Base.id,
// //     },
// //     update: {}
// //   })
//   console.log('Seeding completed successfully.');
// }

// main()
//   .catch((e) => {
//     console.error(e);
//     process.exit(1);
//   })
//   .finally(async () => {
//     await prisma.$disconnect();
//   });
