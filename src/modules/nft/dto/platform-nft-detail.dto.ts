export interface PlatFormNFTDetail1155 {
  asset: Asset;
  status: string;
  type: string;
}

export interface Asset {
  assetId: string;
  tokenId: string;
  attributes: string;
  totalSupply: string;
  assetOwners: AssetOwner[];
}

export interface AssetOwner {
  balance: string;
  createdAt: string;
  id: string;
  owner: string;
  updatedAt: string;
}

export interface PlatFormNFTDetail721 {
  asset: Asset;
  status: string;
  type: string;
}

export interface Asset {
  id: string;
  chainId: number;
  assetId: string;
  tokenId: string;
  owner: string;
  attributes: string;
  createdAt: string;
  updatedAt: string;
}
