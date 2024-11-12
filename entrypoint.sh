#!/bin/sh

# Run database migrations
npx prisma migrate deploy

npx ts-node prisma/seed.ts
# Start the NestJS server
npx prisma studio & yarn start:prod
