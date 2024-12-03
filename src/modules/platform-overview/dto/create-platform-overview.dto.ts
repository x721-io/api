export class CreatePlatformOverviewDto {
  platform: string;
  name: string;
  avatar?: string;
  banner?: string;
  description?: string;
}

export class PlatformOverviewFilter {
  platform?: string;
  templateStatus?: string;
  limit?: string;
  page?: string;
}
