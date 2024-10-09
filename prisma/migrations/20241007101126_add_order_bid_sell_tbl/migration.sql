-- CreateEnum
CREATE TYPE "ORDERSTATUS" AS ENUM ('OPEN', 'CANCELLED', 'FILLED');

-- CreateEnum
CREATE TYPE "ORDERTYPE" AS ENUM ('BID', 'SELL');

-- CreateTable
CREATE TABLE "Order" (
    "orderId" UUID NOT NULL,
    "sig" TEXT NOT NULL,
    "makerId" UUID NOT NULL,
    "makeAssetType" INTEGER NOT NULL,
    "makeAssetAddress" TEXT NOT NULL,
    "makeAssetValue" TEXT NOT NULL,
    "makeAssetId" TEXT NOT NULL,
    "takerId" UUID NOT NULL,
    "takeAssetType" INTEGER NOT NULL,
    "takeAssetAddress" TEXT NOT NULL,
    "takeAssetValue" TEXT NOT NULL,
    "takeAssetId" TEXT NOT NULL,
    "salt" TEXT NOT NULL,
    "start" INTEGER NOT NULL DEFAULT 0,
    "end" INTEGER NOT NULL DEFAULT 0,
    "dataOrder" TEXT NOT NULL,
    "orderStatus" "ORDERSTATUS" NOT NULL DEFAULT 'OPEN',
    "orderType" "ORDERTYPE" NOT NULL,
    "listingRoot" TEXT,
    "proof" TEXT,
    "tokenId" VARCHAR(255) NOT NULL,
    "collectionId" UUID NOT NULL,
    "price" TEXT NOT NULL DEFAULT '0',
    "priceNum" DOUBLE PRECISION NOT NULL DEFAULT 0,
    "netPrice" TEXT NOT NULL DEFAULT '0',
    "netPriceNum" DOUBLE PRECISION NOT NULL DEFAULT 0,
    "createAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3),

    CONSTRAINT "Order_pkey" PRIMARY KEY ("orderId","sig")
);

-- CreateIndex
CREATE INDEX "Order_orderId_sig_idx" ON "Order"("orderId", "sig");

-- AddForeignKey
ALTER TABLE "Order" ADD CONSTRAINT "Order_makerId_fkey" FOREIGN KEY ("makerId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "Order" ADD CONSTRAINT "Order_takerId_fkey" FOREIGN KEY ("takerId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "Order" ADD CONSTRAINT "order_by_id_fk" FOREIGN KEY ("tokenId", "collectionId") REFERENCES "NFT"("id", "collectionId") ON DELETE RESTRICT ON UPDATE CASCADE;
