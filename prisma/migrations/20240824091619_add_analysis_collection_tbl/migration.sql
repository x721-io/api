-- CreateTable
CREATE TABLE "AnalysisCollection" (
    "id" TEXT NOT NULL,
    "collectionId" UUID NOT NULL,
    "keyTime" TEXT NOT NULL,
    "address" TEXT NOT NULL,
    "type" "CONTRACT_TYPE" NOT NULL,
    "volume" DECIMAL(78,0) NOT NULL DEFAULT 0,
    "volumeWei" TEXT NOT NULL DEFAULT '0',
    "floorPrice" BIGINT NOT NULL DEFAULT 0,
    "items" BIGINT NOT NULL DEFAULT 0,
    "owner" BIGINT NOT NULL DEFAULT 0,
    "createdAt" DATE NOT NULL DEFAULT CURRENT_TIMESTAMP,

    CONSTRAINT "AnalysisCollection_pkey" PRIMARY KEY ("id")
);

-- AddForeignKey
ALTER TABLE "AnalysisCollection" ADD CONSTRAINT "AnalysisCollection_collectionId_fkey" FOREIGN KEY ("collectionId") REFERENCES "Collection"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
