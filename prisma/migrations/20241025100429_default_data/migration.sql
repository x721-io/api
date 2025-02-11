-- This is an empty migration.
INSERT INTO "User" 
  (
    id, 
    "email", 
    "username", 
    "signer", 
    "publicKey",
    "updatedAt"
  )
VALUES 
  (
    '6c811c32-39c9-4303-883b-9e9ff5882ed2', 
    'user1@example.com', 
    'user1', 
    '0x2d16d2fc0074fd4c1442258dc6fcba8834a405c5', 
    '0x2d16D2fc0074FD4c1442258DC6fCBa8834a405C5',
    '2024-10-25T06:54:22.253Z'
  ) ON CONFLICT ("signer") DO NOTHING;

INSERT INTO "Collection" 
  (
    id, 
    "txCreationHash", 
    "name", 
    "symbol",
    "status",
    "type",
    "address",
    "updatedAt"
  )
VALUES 
  (
    '5a799476-3129-4066-bd2f-30d2d4f48b3e', 
    '0x195d3eadeb3eb9837fb4d64cb2eccd2dd66b174b10f760508c9998cf23ae1d20', 
    'ERC721Base', 
    'ERC721Base', 
    'SUCCESS',
    'ERC721',
    '0x7ddb1accb3160cf6ba4fee23e26b6d9ad45bc824',
    '2024-10-25T06:54:22.253Z'
  ),
  (
    '849003e2-5010-46d2-bb02-9bdc0dbd1384', 
    '0xb4bda9104f049a6b0383981c06665ca40da15cb95a09b08f916969b6802ec1ea', 
    'ERC1155Base', 
    'ERC1155Base', 
    'SUCCESS',
    'ERC1155',
    '0xda4a022bbc044b8097159c8f4527fe7c6111a70c',
    '2024-10-25T06:54:22.253Z'
  ) ON CONFLICT ("address") DO NOTHING;


  INSERT INTO "UserCollection" 
  (
    "userId",
    "collectionId"
  )   
  VALUES 
    (
        '6c811c32-39c9-4303-883b-9e9ff5882ed2', 
        '5a799476-3129-4066-bd2f-30d2d4f48b3e'
    ),
    (   
        '6c811c32-39c9-4303-883b-9e9ff5882ed2', 
        '849003e2-5010-46d2-bb02-9bdc0dbd1384'
    );
