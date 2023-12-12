-- CreateTable
CREATE TABLE "Project" (
    "id" UUID NOT NULL,
    "idOnchain" INTEGER NOT NULL,
    "name" TEXT NOT NULL,
    "banner" TEXT,
    "description" TEXT,
    "organization" TEXT,
    "website" TEXT,
    "teleLink" TEXT,
    "facebookLink" TEXT,
    "instaLink" TEXT,
    "discordLink" TEXT,
    "shortLink" TEXT,

    CONSTRAINT "Project_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "ProjectRound" (
    "projectId" UUID NOT NULL,
    "roundId" UUID NOT NULL,
    "address" TEXT,

    CONSTRAINT "ProjectRound_pkey" PRIMARY KEY ("projectId","roundId")
);

-- CreateTable
CREATE TABLE "RoundInfo" (
    "id" UUID NOT NULL,
    "name" TEXT,
    "description" TEXT,
    "start" TIMESTAMP(3) NOT NULL,
    "end" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "RoundInfo_pkey" PRIMARY KEY ("id")
);

-- AddForeignKey
ALTER TABLE "ProjectRound" ADD CONSTRAINT "ProjectRound_projectId_fkey" FOREIGN KEY ("projectId") REFERENCES "Project"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "ProjectRound" ADD CONSTRAINT "ProjectRound_roundId_fkey" FOREIGN KEY ("roundId") REFERENCES "RoundInfo"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
