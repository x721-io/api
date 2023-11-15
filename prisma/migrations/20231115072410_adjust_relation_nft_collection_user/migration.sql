/*
  Warnings:

  - You are about to drop the column `creators` on the `Collection` table. All the data in the column will be lost.

*/
-- AlterTable
ALTER TABLE "Collection" DROP COLUMN "creators";

-- CreateTable
CREATE TABLE "UserNFT" (
    "userId" UUID NOT NULL,
    "nftId" VARCHAR(255) NOT NULL,
    "quantity" INTEGER NOT NULL DEFAULT 1,

    CONSTRAINT "UserNFT_pkey" PRIMARY KEY ("userId","nftId")
);

-- CreateTable
CREATE TABLE "UserCollection" (
    "userId" UUID NOT NULL,
    "collectionId" UUID NOT NULL,

    CONSTRAINT "UserCollection_pkey" PRIMARY KEY ("userId","collectionId")
);

-- AddForeignKey
ALTER TABLE "UserNFT" ADD CONSTRAINT "UserNFT_userId_fkey" FOREIGN KEY ("userId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "UserNFT" ADD CONSTRAINT "UserNFT_nftId_fkey" FOREIGN KEY ("nftId") REFERENCES "NFT"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "UserCollection" ADD CONSTRAINT "UserCollection_userId_fkey" FOREIGN KEY ("userId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "UserCollection" ADD CONSTRAINT "UserCollection_collectionId_fkey" FOREIGN KEY ("collectionId") REFERENCES "Collection"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
