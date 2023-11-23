/*
  Warnings:

  - You are about to drop the column `displayType` on the `Trait` table. All the data in the column will be lost.
  - You are about to drop the column `traitType` on the `Trait` table. All the data in the column will be lost.
  - Added the required column `trait_type` to the `Trait` table without a default value. This is not possible if the table is not empty.

*/
-- AlterTable
ALTER TABLE "Trait" DROP COLUMN "displayType",
DROP COLUMN "traitType",
ADD COLUMN     "display_type" TEXT,
ADD COLUMN     "trait_type" TEXT NOT NULL;
