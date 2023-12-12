/*
  Warnings:

  - The primary key for the `ProjectRound` table will be changed. If it partially fails, the table could be left without primary key constraint.
  - The primary key for the `RoundInfo` table will be changed. If it partially fails, the table could be left without primary key constraint.
  - You are about to drop the column `end` on the `RoundInfo` table. All the data in the column will be lost.
  - You are about to drop the column `start` on the `RoundInfo` table. All the data in the column will be lost.
  - The `id` column on the `RoundInfo` table would be dropped and recreated. This will lead to data loss if there is data in the column.
  - Added the required column `end` to the `ProjectRound` table without a default value. This is not possible if the table is not empty.
  - Added the required column `start` to the `ProjectRound` table without a default value. This is not possible if the table is not empty.
  - Changed the type of `roundId` on the `ProjectRound` table. No cast exists, the column would be dropped and recreated, which cannot be done if there is data, since the column is required.

*/
-- DropForeignKey
ALTER TABLE "ProjectRound" DROP CONSTRAINT "ProjectRound_roundId_fkey";

-- AlterTable
ALTER TABLE "ProjectRound" DROP CONSTRAINT "ProjectRound_pkey",
ADD COLUMN     "end" TIMESTAMP(3) NOT NULL,
ADD COLUMN     "start" TIMESTAMP(3) NOT NULL,
DROP COLUMN "roundId",
ADD COLUMN     "roundId" INTEGER NOT NULL,
ADD CONSTRAINT "ProjectRound_pkey" PRIMARY KEY ("projectId", "roundId");

-- AlterTable
ALTER TABLE "RoundInfo" DROP CONSTRAINT "RoundInfo_pkey",
DROP COLUMN "end",
DROP COLUMN "start",
DROP COLUMN "id",
ADD COLUMN     "id" SERIAL NOT NULL,
ADD CONSTRAINT "RoundInfo_pkey" PRIMARY KEY ("id");

-- AddForeignKey
ALTER TABLE "ProjectRound" ADD CONSTRAINT "ProjectRound_roundId_fkey" FOREIGN KEY ("roundId") REFERENCES "RoundInfo"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
