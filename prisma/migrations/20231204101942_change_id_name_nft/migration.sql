/*
  Warnings:

  - You are about to drop the column `rId` on the `NFT` table. All the data in the column will be lost.
  - Added the required column `u2uId` to the `NFT` table without a default value. This is not possible if the table is not empty.

*/
-- AlterTable
ALTER TABLE "NFT" DROP COLUMN "rId",
ADD COLUMN     "u2uId" VARCHAR(255) NOT NULL;
