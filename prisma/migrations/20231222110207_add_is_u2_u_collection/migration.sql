/*
  Warnings:

  - You are about to drop the column `isU2U` on the `NFT` table. All the data in the column will be lost.

*/
-- AlterTable
ALTER TABLE "Collection" ADD COLUMN     "isU2U" BOOLEAN NOT NULL DEFAULT true;

-- AlterTable
ALTER TABLE "NFT" DROP COLUMN "isU2U";
