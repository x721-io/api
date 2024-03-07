interface PagingResponseHasNext<T> {
  data: T[];
  paging: {
    limit: number;
    page: number;
    hasNext: boolean;
  };
}
