#!/bin/sh

# Run database migrations
npx prisma migrate deploy

# Start the NestJS server
npx prisma studio & yarn start:staging
