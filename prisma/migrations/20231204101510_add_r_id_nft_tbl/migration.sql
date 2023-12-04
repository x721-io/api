/*
  Warnings:

  - Added the required column `rId` to the `NFT` table without a default value. This is not possible if the table is not empty.

*/
-- AlterTable
ALTER TABLE "NFT" ADD COLUMN     "rId" VARCHAR(255) NOT NULL;
