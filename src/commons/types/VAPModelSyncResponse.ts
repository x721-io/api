export interface VAPResponse {
  data: CarBrandsModels;
}

export type CarBrandsModels = { [brand: string]: readonly string[] };
