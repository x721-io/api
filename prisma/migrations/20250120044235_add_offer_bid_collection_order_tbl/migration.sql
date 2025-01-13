-- AlterEnum
ALTER TYPE "ORDERTYPE" ADD VALUE 'BID_COLLECTION';

-- AlterTable
ALTER TABLE "Order" ADD COLUMN     "idxOffer" INTEGER DEFAULT 1;

-- AlterTable
ALTER TABLE "OrderHistory" ADD COLUMN     "idxOffer" INTEGER DEFAULT 1;

-- CreateTable
CREATE TABLE "Offer" (
    "index" INTEGER NOT NULL DEFAULT 1,
    "idxOffer" INTEGER DEFAULT 1,
    "sig" TEXT NOT NULL,
    "makerId" UUID NOT NULL,
    "makeAssetType" INTEGER NOT NULL,
    "makeAssetAddress" TEXT NOT NULL,
    "makeAssetValue" TEXT NOT NULL,
    "makeAssetId" TEXT NOT NULL,
    "takeAssetType" INTEGER NOT NULL,
    "takeAssetAddress" TEXT NOT NULL,
    "takeAssetValue" TEXT NOT NULL,
    "takeAssetId" TEXT,
    "salt" TEXT NOT NULL,
    "start" INTEGER NOT NULL DEFAULT 0,
    "end" INTEGER NOT NULL DEFAULT 0,
    "offerStatus" "ORDERSTATUS" NOT NULL DEFAULT 'OPEN',
    "offerType" "ORDERTYPE" NOT NULL,
    "root" TEXT NOT NULL DEFAULT '0x0000000000000000000000000000000000000000000000000000000000000000',
    "proof" TEXT[] DEFAULT ARRAY[]::TEXT[],
    "tokenId" VARCHAR(255),
    "collectionId" UUID NOT NULL,
    "quantity" INTEGER NOT NULL DEFAULT 1,
    "price" TEXT NOT NULL DEFAULT '0',
    "priceNum" DOUBLE PRECISION NOT NULL DEFAULT 0,
    "netPrice" TEXT NOT NULL DEFAULT '0',
    "netPriceNum" DOUBLE PRECISION NOT NULL DEFAULT 0,
    "createAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3),
    "quoteToken" TEXT NOT NULL,
    "filledQty" INTEGER NOT NULL DEFAULT 0,

    CONSTRAINT "Offer_pkey" PRIMARY KEY ("sig","index")
);

-- CreateIndex
CREATE INDEX "Offer_sig_index_idx" ON "Offer"("sig", "index");

-- CreateIndex
CREATE INDEX "Order_sig_idxOffer_idx" ON "Order"("sig", "idxOffer");

-- CreateIndex
CREATE INDEX "OrderHistory_sig_idxOffer_nonce_idx" ON "OrderHistory"("sig", "idxOffer", "nonce");

-- AddForeignKey
ALTER TABLE "Offer" ADD CONSTRAINT "Offer_makerId_fkey" FOREIGN KEY ("makerId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
