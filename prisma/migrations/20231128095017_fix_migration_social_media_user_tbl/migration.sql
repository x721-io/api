/*
  Warnings:

  - You are about to drop the column `bio` on the `User` table. All the data in the column will be lost.

*/
-- AlterTable
ALTER TABLE "User" DROP COLUMN "bio",
ALTER COLUMN "discordLink" SET DATA TYPE TEXT,
ALTER COLUMN "facebookLink" SET DATA TYPE TEXT,
ALTER COLUMN "telegramLink" SET DATA TYPE TEXT,
ALTER COLUMN "twitterLink" SET DATA TYPE TEXT,
ALTER COLUMN "webURL" SET DATA TYPE TEXT;
