/*
  Warnings:

  - The primary key for the `NFT` table will be changed. If it partially fails, the table could be left without primary key constraint.
  - A unique constraint covering the columns `[address]` on the table `Collection` will be added. If there are existing duplicate values, this will fail.
  - Added the required column `collectionId` to the `Trait` table without a default value. This is not possible if the table is not empty.
  - Added the required column `collectionId` to the `UserNFT` table without a default value. This is not possible if the table is not empty.

*/
-- DropForeignKey
ALTER TABLE "Trait" DROP CONSTRAINT "Trait_nftId_fkey";

-- DropForeignKey
ALTER TABLE "UserNFT" DROP CONSTRAINT "UserNFT_nftId_fkey";

-- AlterTable
ALTER TABLE "NFT" DROP CONSTRAINT "NFT_pkey",
ADD CONSTRAINT "NFT_pkey" PRIMARY KEY ("id", "collectionId");

-- AlterTable
ALTER TABLE "Trait" ADD COLUMN     "collectionId" UUID NOT NULL;

-- AlterTable
ALTER TABLE "UserNFT" ADD COLUMN     "collectionId" UUID NOT NULL;

-- CreateIndex
CREATE UNIQUE INDEX "Collection_address_key" ON "Collection"("address");

-- AddForeignKey
ALTER TABLE "Trait" ADD CONSTRAINT "Trait_nftId_collectionId_fkey" FOREIGN KEY ("nftId", "collectionId") REFERENCES "NFT"("id", "collectionId") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "UserNFT" ADD CONSTRAINT "UserNFT_nftId_collectionId_fkey" FOREIGN KEY ("nftId", "collectionId") REFERENCES "NFT"("id", "collectionId") ON DELETE RESTRICT ON UPDATE CASCADE;
