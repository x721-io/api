interface OAuthZaloRes {
  access_token: string;
  refresh_token: string;
  expires_in: string;
}

interface OAuthZaloReq {
  code?: string;
  app_id: string;
  grant_type: 'authorization_code' | 'refresh_token';
  code_verifier?: string;
  refresh_token?: string;
}
