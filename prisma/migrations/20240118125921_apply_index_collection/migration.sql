/*
  Warnings:

  - A unique constraint covering the columns `[id,address]` on the table `Collection` will be added. If there are existing duplicate values, this will fail.

*/
-- CreateIndex
CREATE INDEX "Collection_address_idx" ON "Collection"("address");

-- CreateIndex
CREATE UNIQUE INDEX "Collection_id_address_key" ON "Collection"("id", "address");
