-- CreateIndex
CREATE INDEX "NFT_u2uId_collectionId_idx" ON "NFT"("u2uId", "collectionId");

-- CreateIndex
CREATE INDEX "NFT_collectionId_idx" ON "NFT"("collectionId");

-- CreateIndex
CREATE INDEX "NFT_creatorId_idx" ON "NFT"("creatorId");

-- CreateIndex
CREATE INDEX "Trait_nftId_collectionId_idx" ON "Trait"("nftId", "collectionId");

-- CreateIndex
CREATE INDEX "Trait_display_type_idx" ON "Trait"("display_type");

-- CreateIndex
CREATE INDEX "Trait_value_idx" ON "Trait"("value");
