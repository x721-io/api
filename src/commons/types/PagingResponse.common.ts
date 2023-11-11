interface PagingResponse<T> {
  data: T[];
  paging: {
    total: number;
    limit: number;
    page: number;
  };
}
