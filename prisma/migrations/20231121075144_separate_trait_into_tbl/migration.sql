/*
  Warnings:

  - You are about to drop the column `traits` on the `NFT` table. All the data in the column will be lost.

*/
-- AlterTable
ALTER TABLE "NFT" DROP COLUMN "traits";

-- CreateTable
CREATE TABLE "Trait" (
    "id" UUID NOT NULL,
    "traitType" TEXT NOT NULL,
    "displayType" TEXT,
    "value" TEXT NOT NULL,
    "nftId" TEXT NOT NULL,

    CONSTRAINT "Trait_pkey" PRIMARY KEY ("id")
);

-- AddForeignKey
ALTER TABLE "Trait" ADD CONSTRAINT "Trait_nftId_fkey" FOREIGN KEY ("nftId") REFERENCES "NFT"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
