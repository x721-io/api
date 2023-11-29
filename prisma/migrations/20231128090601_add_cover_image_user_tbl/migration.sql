/*
  Warnings:

  - You are about to alter the column `discordLink` on the `User` table. The data in that column could be lost. The data in that column will be cast from `VarChar(1000)` to `VarChar(255)`.
  - You are about to alter the column `facebookLink` on the `User` table. The data in that column could be lost. The data in that column will be cast from `VarChar(1000)` to `VarChar(255)`.
  - You are about to alter the column `telegramLink` on the `User` table. The data in that column could be lost. The data in that column will be cast from `VarChar(1000)` to `VarChar(255)`.
  - You are about to alter the column `twitterLink` on the `User` table. The data in that column could be lost. The data in that column will be cast from `VarChar(1000)` to `VarChar(255)`.
  - You are about to alter the column `webURL` on the `User` table. The data in that column could be lost. The data in that column will be cast from `VarChar(1000)` to `VarChar(255)`.

*/
-- AlterTable
ALTER TABLE "User" ADD COLUMN     "coverImage" TEXT,
ALTER COLUMN "discordLink" SET DATA TYPE VARCHAR(255),
ALTER COLUMN "facebookLink" SET DATA TYPE VARCHAR(255),
ALTER COLUMN "telegramLink" SET DATA TYPE VARCHAR(255),
ALTER COLUMN "twitterLink" SET DATA TYPE VARCHAR(255),
ALTER COLUMN "webURL" SET DATA TYPE VARCHAR(255);
