export enum TypeCategory {
  Collection = 'Collection',
  User = 'User',
  NFT = 'NFT',
}

export enum MetricCategory {
  Verified = 'Verified',
  Volume = 'Volume',
  TotalUniqueOwner = 'TotalUniqueOwner',
  TotalItems = 'TotalItems',
  Followers = 'Followers',
  CollectionMetric = 'CollectionMetric',
  UserMetric = 'UserMetric',
  VolumeIndividual = 'VolumeIndividual',
}

export interface Point {
  point: number;
}

export interface GeneralPoint {
  key: string;
  point: number;
  value: any;
  total?: number;
}

export class MetricUser {
  Verified: Point;
  Followers: GeneralPoint[];
}

export class MetricUserJson {
  Verified: number;
  Followers: GeneralPoint;
  CollectionMetric: string;
  VolumeIndividual: string;
}

export class MetricCollection {
  Verified: Point;
  Followers: GeneralPoint[];
  TotalUniqueOwner: GeneralPoint[];
  TotalItems: GeneralPoint[];
  Volume: GeneralPoint[];
}

export class MetricCollectionJson {
  Verified: number;
  Volume: GeneralPoint;
  TotalUniqueOwner: GeneralPoint;
  TotalItems: GeneralPoint;
  Followers: GeneralPoint;
}

export class MetricNFT {
  VolumeIndividual: GeneralPoint[];
}

export class MetricNFTJson {
  UserMetric: string;
  VolumeIndividual: string;
}
