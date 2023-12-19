/*
  Warnings:

  - You are about to drop the column `discordLink` on the `Project` table. All the data in the column will be lost.
  - You are about to drop the column `facebookLink` on the `Project` table. All the data in the column will be lost.
  - You are about to drop the column `instaLink` on the `Project` table. All the data in the column will be lost.
  - You are about to drop the column `teleLink` on the `Project` table. All the data in the column will be lost.

*/
-- AlterTable
ALTER TABLE "Project" DROP COLUMN "discordLink",
DROP COLUMN "facebookLink",
DROP COLUMN "instaLink",
DROP COLUMN "teleLink",
ADD COLUMN     "details" JSONB[] DEFAULT ARRAY[]::JSONB[],
ADD COLUMN     "discord" TEXT,
ADD COLUMN     "facebook" TEXT,
ADD COLUMN     "instagram" TEXT,
ADD COLUMN     "logo" TEXT,
ADD COLUMN     "telegram" TEXT,
ADD COLUMN     "twitter" TEXT;
