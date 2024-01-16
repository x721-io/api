-- CreateTable
CREATE TABLE "RoundRangeTime" (
    "id" UUID NOT NULL,
    "roundId" INTEGER NOT NULL,
    "projectId" UUID NOT NULL,
    "start" TIMESTAMP(3) NOT NULL,
    "end" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "RoundRangeTime_pkey" PRIMARY KEY ("id")
);

-- AddForeignKey
ALTER TABLE "RoundRangeTime" ADD CONSTRAINT "RoundRangeTime_roundId_fkey" FOREIGN KEY ("roundId") REFERENCES "RoundInfo"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "RoundRangeTime" ADD CONSTRAINT "RoundRangeTime_projectId_fkey" FOREIGN KEY ("projectId") REFERENCES "Project"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
