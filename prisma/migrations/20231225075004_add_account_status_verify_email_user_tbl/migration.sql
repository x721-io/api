-- AlterTable
ALTER TABLE "User" ADD COLUMN     "accountStatus" BOOLEAN NOT NULL DEFAULT false,
ADD COLUMN     "verifyEmail" BOOLEAN NOT NULL DEFAULT false;
