# csv - Erlang library for parsing CSV files.

RFC 4180 compliant CSV parser.

## Features

  - Handles utf8 data (RFC 4180 only defines 7-bit ascii).
  - Accepts lines ending in LF only (RFC 4180 only defines CR LF as
    line ending).
  - Configurable separator character, default comma (,).
  - Can be configured to ignore comments, i.e., lines starting with hash (#).
  - Default is non-strict mode, in which it ignores comments and allows
    double quotes (") in non-quoted fields.

NOTE: does not validate that all records have equal number of fields.

## API

### `read_file/1`

```
-spec read_file(FName :: string()) ->
          {ok, [list(binary())]} | {error, term()}.
```

### `read_file/2`

```
-spec read_file(FName :: string(),
                Opts :: #{separator => char(),
                          strict => boolean(),
                          comments => boolean()}) ->
          {ok, [list(binary())]} | {error, term()}.
```

### `format_error/1`

```
-spec format_error(term()) -> string().
```
