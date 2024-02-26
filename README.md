<p align="center">
  <a href="http://nestjs.com/" target="blank"><img src="https://nestjs.com/img/logo-small.svg" width="200" alt="Nest Logo" /></a>
</p>

[circleci-image]: https://img.shields.io/circleci/build/github/nestjs/nest/master?token=abc123def456
[circleci-url]: https://circleci.com/gh/nestjs/nest

  <p align="center">A progressive <a href="http://nodejs.org" target="_blank">Node.js</a> framework for building efficient and scalable server-side applications.</p>
    <p align="center">
<a href="https://www.npmjs.com/~nestjscore" target="_blank"><img src="https://img.shields.io/npm/v/@nestjs/core.svg" alt="NPM Version" /></a>
<a href="https://www.npmjs.com/~nestjscore" target="_blank"><img src="https://img.shields.io/npm/l/@nestjs/core.svg" alt="Package License" /></a>
<a href="https://www.npmjs.com/~nestjscore" target="_blank"><img src="https://img.shields.io/npm/dm/@nestjs/common.svg" alt="NPM Downloads" /></a>
<a href="https://circleci.com/gh/nestjs/nest" target="_blank"><img src="https://img.shields.io/circleci/build/github/nestjs/nest/master" alt="CircleCI" /></a>
<a href="https://coveralls.io/github/nestjs/nest?branch=master" target="_blank"><img src="https://coveralls.io/repos/github/nestjs/nest/badge.svg?branch=master#9" alt="Coverage" /></a>
<a href="https://discord.gg/G7Qnnhy" target="_blank"><img src="https://img.shields.io/badge/discord-online-brightgreen.svg" alt="Discord"/></a>
<a href="https://opencollective.com/nest#backer" target="_blank"><img src="https://opencollective.com/nest/backers/badge.svg" alt="Backers on Open Collective" /></a>
<a href="https://opencollective.com/nest#sponsor" target="_blank"><img src="https://opencollective.com/nest/sponsors/badge.svg" alt="Sponsors on Open Collective" /></a>
  <a href="https://paypal.me/kamilmysliwiec" target="_blank"><img src="https://img.shields.io/badge/Donate-PayPal-ff3f59.svg"/></a>
    <a href="https://opencollective.com/nest#sponsor"  target="_blank"><img src="https://img.shields.io/badge/Support%20us-Open%20Collective-41B883.svg" alt="Support us"></a>
  <a href="https://twitter.com/nestframework" target="_blank"><img src="https://img.shields.io/twitter/follow/nestframework.svg?style=social&label=Follow"></a>
</p>
  <!--[![Backers on Open Collective](https://opencollective.com/nest/backers/badge.svg)](https://opencollective.com/nest#backer)
  [![Sponsors on Open Collective](https://opencollective.com/nest/sponsors/badge.svg)](https://opencollective.com/nest#sponsor)-->

## Description

<!-- [Nest](https://github.com/nestjs/nest) framework TypeScript starter repository. -->
The NFT Marketplace API provides methods to combine smart contracts for minting and exchanging tokens, as well as APIs for order creation and discovery. It encompasses standards used in smart contracts to ensure interoperability and seamless interaction within the marketplace ecosystem.

## Installation

```bash
$ yarn install
```

## Usage
First, you need to set the below environment variables in the `.env` file or your deployment platforms. You can refer to the env.example file


## Running migration database

```bash
# migration model to database
$ npx prisma migrate dev

# or 
$ prisma migrate dev --name name_migration

# If you need to modify a migration before applying it 
  # You want to introduce a significant refactor
  # You want to rename a field (by default, Prisma Migrate will drop the existing field)
  # You want to change the direction of a 1-1 relationship
  # You want to add features that cannot be represented in Prisma Schema Language - such as a partial index or a stored procedure.
# The --create-only command allows you to create a migration without applying it
$ npx prisma migrate dev --create-only

# generate Prisma Client to ensure the code inside `node_modules/.prisma/client` gets updated
$ npx prisma generate
```

## Running generate model schema.graphql to graphql.ts

```bash
# generate model from schema.graphql to graphql.ts
$ yarn codegen
```

## Running the app

```bash
# development
$ yarn run start

# watch mode
$ yarn run start:dev

# production mode
$ yarn run start:prod
```

## Test

```bash
# unit tests
$ yarn run test

# e2e tests
$ yarn run test:e2e

# test coverage
$ yarn run test:cov
```

## 📖 Learn More
- [Nest](https://github.com/nestjs/nest): Nest is a framework for building efficient, scalable <a href="https://nodejs.org" target="_blank">Node.js</a> server-side applications.
- [Prisma](https://github.com/prisma/prisma): Prisma is a **next-generation ORM** that consists of these tools:
  - [**Prisma Client**](https://www.prisma.io/docs/concepts/components/prisma-client): Auto-generated and type-safe query builder for Node.js & TypeScript
  - [**Prisma Migrate**](https://www.prisma.io/docs/concepts/components/prisma-migrate): Declarative data modeling & migration system
  - [**Prisma Studio**](https://github.com/prisma/studio): GUI to view and edit data in your database
- [PostgreSQL](https://www.postgresql.org/): PostgreSQL is an advanced object-relational database management system
that supports an extended subset of the SQL standard, including
transactions, foreign keys, subqueries, triggers, user-defined types
and functions.
- [Typescript](https://www.typescriptlang.org/):Typescript is a language for application-scale JavaScript. TypeScript adds optional types to JavaScript that support tools for large-scale JavaScript applications for any browser, for any host, on any OS.
- [GraphQL Client](https://github.com/prisma-labs/graphql-request#graphql-request) + [Code Generation](https://www.graphql-code-generator.com/): GraphQL is a data query language developed by Facebook. It provides an alternative to REST and ad-hoc webservice architectures.


## Support

Nest is an MIT-licensed open source project. It can grow thanks to the sponsors and support by the amazing backers. If you'd like to join them, please [read more here](https://docs.nestjs.com/support).

## Stay in touch

- Author - [Kamil Myśliwiec](https://kamilmysliwiec.com)
- Website - [https://nestjs.com](https://nestjs.com/)
- Twitter - [@nestframework](https://twitter.com/nestframework)

## License

Nest is [MIT licensed](LICENSE).
