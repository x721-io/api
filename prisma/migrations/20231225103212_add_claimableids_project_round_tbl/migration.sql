-- AlterTable
ALTER TABLE "ProjectRound" ADD COLUMN     "claimableIds" TEXT[] DEFAULT ARRAY[]::TEXT[];
