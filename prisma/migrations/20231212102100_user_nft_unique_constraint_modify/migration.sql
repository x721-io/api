/*
  Warnings:

  - The primary key for the `UserNFT` table will be changed. If it partially fails, the table could be left without primary key constraint.

*/
-- AlterTable
ALTER TABLE "UserNFT" DROP CONSTRAINT "UserNFT_pkey",
ADD CONSTRAINT "UserNFT_pkey" PRIMARY KEY ("userId", "nftId", "collectionId");
