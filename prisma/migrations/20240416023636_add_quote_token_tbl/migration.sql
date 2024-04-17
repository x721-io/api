-- CreateTable
CREATE TABLE "QuoteTokens" (
    "id" UUID NOT NULL,
    "address" TEXT NOT NULL,
    "symbol" TEXT NOT NULL,
    "name" TEXT NOT NULL,
    "derivedETH" DOUBLE PRECISION NOT NULL,
    "derivedUSD" DOUBLE PRECISION NOT NULL,

    CONSTRAINT "QuoteTokens_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE UNIQUE INDEX "QuoteTokens_address_key" ON "QuoteTokens"("address");
