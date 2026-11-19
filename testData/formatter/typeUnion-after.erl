-type api_error() :: error_reason()
  | {error, error_reason()}
  | {error, [error_reason()]}
  | {error, error_reason(), binary()}.
