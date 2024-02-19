/*
  Warnings:

  - The `floorPrice` column on the `Collection` table would be dropped and recreated. This will lead to data loss if there is data in the column.

*/
-- AlterTable
ALTER TABLE "Collection" DROP COLUMN "floorPrice",
ADD COLUMN     "floorPrice" BIGINT NOT NULL DEFAULT 0;
