export class CreatePlatformTemplateDto {
  nameSlug: string;
  name: string;
  avatar?: string;
  banner?: string;
  description?: string;
  sections: string;
  isActive: boolean;
}
