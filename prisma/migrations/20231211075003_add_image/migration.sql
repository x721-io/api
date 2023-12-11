/*
  Warnings:

  - You are about to drop the column `imageHash` on the `NFT` table. All the data in the column will be lost.

*/
-- AlterTable
ALTER TABLE "NFT" RENAME COLUMN "imageHash" TO "image";
