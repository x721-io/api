-- DropIndex
DROP INDEX "Collection_address_idx";

-- DropIndex
DROP INDEX "Collection_id_address_key";

-- CreateIndex
CREATE INDEX "Collection_address_idx" ON "Collection" USING HASH ("address");
