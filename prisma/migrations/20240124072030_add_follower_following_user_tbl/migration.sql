-- AlterTable
ALTER TABLE "User" ADD COLUMN     "followers" INTEGER DEFAULT 0,
ADD COLUMN     "following" INTEGER DEFAULT 0;

-- CreateTable
CREATE TABLE "UserFollow" (
    "userId" UUID NOT NULL,
    "followerId" UUID NOT NULL,
    "isFollow" BOOLEAN NOT NULL DEFAULT true,

    CONSTRAINT "UserFollow_pkey" PRIMARY KEY ("userId","followerId")
);

-- AddForeignKey
ALTER TABLE "UserFollow" ADD CONSTRAINT "UserFollow_userId_fkey" FOREIGN KEY ("userId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "UserFollow" ADD CONSTRAINT "UserFollow_followerId_fkey" FOREIGN KEY ("followerId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
