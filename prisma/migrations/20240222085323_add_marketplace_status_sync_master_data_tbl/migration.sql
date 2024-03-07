-- CreateEnum
CREATE TYPE "SELL_STATUS" AS ENUM ('AskNew', 'AskCancel', 'Trade', 'AcceptBid', 'Bid', 'CancelBid');

-- CreateTable
CREATE TABLE "MarketplaceStatus" (
    "id" SERIAL NOT NULL,
    "tokenId" VARCHAR(255) NOT NULL,
    "collectionId" UUID NOT NULL,
    "quoteToken" VARCHAR(255),
    "timestamp" INTEGER NOT NULL,
    "price" DOUBLE PRECISION NOT NULL,
    "priceWei" TEXT NOT NULL,
    "netPrice" DOUBLE PRECISION NOT NULL,
    "netPriceWei" TEXT NOT NULL,
    "event" "SELL_STATUS" NOT NULL,
    "quantity" INTEGER NOT NULL DEFAULT 1,
    "operationId" TEXT,
    "txHash" TEXT,
    "operation" TEXT,
    "from" TEXT,
    "askId" TEXT,

    CONSTRAINT "MarketplaceStatus_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "SyncMasterData" (
    "timestamp" INTEGER NOT NULL DEFAULT 0,
    "type" "CONTRACT_TYPE" NOT NULL,

    CONSTRAINT "SyncMasterData_pkey" PRIMARY KEY ("type")
);

-- CreateIndex
CREATE INDEX "MarketplaceStatus_tokenId_collectionId_idx" ON "MarketplaceStatus"("tokenId", "collectionId");

-- AddForeignKey
ALTER TABLE "MarketplaceStatus" ADD CONSTRAINT "marketplace_by_id_fk" FOREIGN KEY ("tokenId", "collectionId") REFERENCES "NFT"("id", "collectionId") ON DELETE RESTRICT ON UPDATE CASCADE;
