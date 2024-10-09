/*
  Warnings:

  - The primary key for the `SyncMasterData` table will be changed. If it partially fails, the table could be left without primary key constraint.
  - Changed the type of `type` on the `SyncMasterData` table. No cast exists, the column would be dropped and recreated, which cannot be done if there is data, since the column is required.

*/
-- CreateEnum
CREATE TYPE "ORDERSTATUS" AS ENUM ('OPEN', 'PENDING', 'CANCELLED', 'FILLED');

-- CreateEnum
CREATE TYPE "ORDERTYPE" AS ENUM ('BID', 'BULK', 'SINGLE');

-- CreateEnum
CREATE TYPE "SYNCDATASTATUS" AS ENUM ('ERC1155', 'ERC721', 'ORDER');

-- AlterTable
ALTER TABLE "SyncMasterData" DROP CONSTRAINT "SyncMasterData_pkey",
DROP COLUMN "type",
ADD COLUMN     "type" "SYNCDATASTATUS" NOT NULL,
ADD CONSTRAINT "SyncMasterData_pkey" PRIMARY KEY ("type");

-- CreateTable
CREATE TABLE "Order" (
    "index" INTEGER NOT NULL DEFAULT 1,
    "sig" TEXT NOT NULL,
    "makerId" UUID NOT NULL,
    "makeAssetType" INTEGER NOT NULL,
    "makeAssetAddress" TEXT NOT NULL,
    "makeAssetValue" TEXT NOT NULL,
    "makeAssetId" TEXT NOT NULL,
    "takerId" UUID,
    "takeAssetType" INTEGER NOT NULL,
    "takeAssetAddress" TEXT NOT NULL,
    "takeAssetValue" TEXT NOT NULL,
    "takeAssetId" TEXT NOT NULL,
    "salt" TEXT NOT NULL,
    "start" INTEGER NOT NULL DEFAULT 0,
    "end" INTEGER NOT NULL DEFAULT 0,
    "orderStatus" "ORDERSTATUS" NOT NULL DEFAULT 'OPEN',
    "orderType" "ORDERTYPE" NOT NULL,
    "root" TEXT NOT NULL DEFAULT '0x0000000000000000000000000000000000000000000000000000000000000000',
    "proof" TEXT[] DEFAULT ARRAY[]::TEXT[],
    "tokenId" VARCHAR(255) NOT NULL,
    "collectionId" UUID NOT NULL,
    "quantity" INTEGER NOT NULL DEFAULT 1,
    "price" TEXT NOT NULL DEFAULT '0',
    "priceNum" DOUBLE PRECISION NOT NULL DEFAULT 0,
    "netPrice" TEXT NOT NULL DEFAULT '0',
    "netPriceNum" DOUBLE PRECISION NOT NULL DEFAULT 0,
    "createAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3),
    "quoteToken" TEXT NOT NULL,
    "filled" INTEGER NOT NULL DEFAULT 0,

    CONSTRAINT "Order_pkey" PRIMARY KEY ("sig","index")
);

-- CreateIndex
CREATE INDEX "Order_sig_index_idx" ON "Order"("sig", "index");

-- AddForeignKey
ALTER TABLE "Order" ADD CONSTRAINT "Order_makerId_fkey" FOREIGN KEY ("makerId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "Order" ADD CONSTRAINT "Order_takerId_fkey" FOREIGN KEY ("takerId") REFERENCES "User"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "Order" ADD CONSTRAINT "order_by_id_fk" FOREIGN KEY ("tokenId", "collectionId") REFERENCES "NFT"("id", "collectionId") ON DELETE RESTRICT ON UPDATE CASCADE;
