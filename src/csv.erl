%%% RFC 4180 compliant CSV parser.
%%%
%%% Handles utf8 data (RFC 4180 only handles 7-bit ascii).
%%% Accepts lines ending in LF only (i.e., not CR LF).
%%% Configurable separator character, default comma (,).
%%% Can be configured to ignore comments, i.e., lines starting with hash (#).
%%% Default is non-strict mode, in which it ignores comments and allows
%%%   double quotes (") in non-quoted fields.
%%% NOTE: does not validate that all records have equal number of fields.

-module(csv).

-export([read_file/1, read_file/2, format_error/1]).

-record(opts, {separator, comments, strict}).

-spec read_file(FName :: string()) ->
          {ok, [list(binary())]} | {error, term()}.
read_file(FName) ->
    read_file(FName, #{}).

-spec read_file(FName :: string(),
                Opts :: #{separator => char(),
                          strict => boolean(),
                          comments => boolean()}) ->
          {ok, [list(binary())]} | {error, term()}.
%% Consider using unicode:characters_to_list() on returned binaries.
read_file(FName, OptsMap) ->
    Separator = maps:get(separator, OptsMap, $,),
    Strict = maps:get(strict, OptsMap, false),
    Comments = maps:get(comments, OptsMap, not Strict),
    case file:read_file(FName) of
        {ok, <<>>} ->
            {error, empty_file};
        {ok, Data} ->
            Opts = #opts{separator = Separator,
                         comments = Comments,
                         strict = Strict},
            p_fields(unicode:characters_to_list(Data), Opts, 1, [], []);
        {error, Error} ->
            {error, {file, Error}}
    end.

p_fields(L, Opts, Line, FsAcc, RsAcc) ->
    #opts{comments = Comments, separator = Sep} = Opts,
    case L of
        [Sep | T] ->
            p_fields(T, Opts, Line, [<<"">> | FsAcc], RsAcc);
        [$\r, $\n]->
            return([<<"">> | FsAcc], RsAcc);
        [$\r, $\n | T] ->
            p_fields(T, Opts, Line+1, [],
                     [lists:reverse([<<"">> | FsAcc]) | RsAcc]);
        [$\n] ->
            return([<<"">> | FsAcc], RsAcc);
        [$\n | T] ->
            p_fields(T, Opts, Line+1, [],
                     [lists:reverse([<<"">> | FsAcc]) | RsAcc]);
        [$# | T] when FsAcc == [] andalso Comments ->
            p_fields(skip_to_eol(T), Opts, Line+1, [], RsAcc);
        [$" | T] ->
            p_quoted_field(T, [], Opts, Line, Line, FsAcc, RsAcc);
        [H | T] ->
            p_field(T, [H], Opts, Line, FsAcc, RsAcc);
        [] ->
            return(FsAcc, RsAcc)
    end.

p_field(L, Acc, Opts, Line, FsAcc, RsAcc) ->
    #opts{strict = Strict, separator = Sep} = Opts,
    case L of
        [Sep] ->
            return([<<"">>, brev(Acc) | FsAcc], RsAcc);
        [Sep | T] ->
            p_fields(T, Opts, Line, [brev(Acc) | FsAcc], RsAcc);
        [$\r, $\n | T] ->
            p_fields(T, Opts, Line+1, [],
                     [lists:reverse([brev(Acc) | FsAcc]) | RsAcc]);
        [$\n | T] ->
            p_fields(T, Opts, Line+1, [],
                     [lists:reverse([brev(Acc) | FsAcc]) | RsAcc]);
        [$" | _] when Strict ->
            {error, {unescaped_quote, Line}};
        [H | T] ->
            p_field(T, [H | Acc], Opts, Line, FsAcc, RsAcc);
        [] ->
            return([brev(Acc) | FsAcc], RsAcc)
    end.

p_quoted_field(L, Acc, Opts, StartL, Line, FsAcc, RsAcc) ->
    #opts{separator = Sep} = Opts,
    case L of
        [$", $" | T] ->
            p_quoted_field(T, [$" | Acc], Opts, StartL, Line, FsAcc, RsAcc);
        [$"] ->
            return([brev(Acc) | FsAcc], RsAcc);
        [$", Sep] ->
            return([<<"">>, brev(Acc) | FsAcc], RsAcc);
        [$", Sep | T] ->
            p_fields(T, Opts, Line, [brev(Acc) | FsAcc], RsAcc);
        [$", $\r, $\n | T] ->
            p_fields(T, Opts, Line+1, [],
                     [lists:reverse([brev(Acc) | FsAcc]) | RsAcc]);
        [$", $\n | T] ->
            p_fields(T, Opts, Line+1, [],
                     [lists:reverse([brev(Acc) | FsAcc]) | RsAcc]);
        [$", Ch | _] ->
            {error, {bad_quote_end, Ch, Line}};
        [$\n | T] ->
            p_quoted_field(T, [$\n | Acc], Opts, StartL, Line+1, FsAcc, RsAcc);
        [H | T] ->
            p_quoted_field(T, [H | Acc], Opts, StartL, Line, FsAcc, RsAcc);
        [] ->
            {error, {runaway_quote, StartL}}
    end.

skip_to_eol(L) ->
    case L of
        [$\r, $\n | T] ->
            T;
        [$\n | T] ->
            T;
        [_ | T] ->
            skip_to_eol(T);
        [] ->
            []
    end.

-spec format_error(term()) -> string().
format_error(Error) ->
    case Error of
        empty_file ->
            "file is empty";
        {unescaped_quote, Line} ->
            io_lib:format("~w: unescaped double quote in field", [Line]);
        {bad_quote_end, Ch, Line} ->
            io_lib:format("~w: unexpected character after quote ~s",
                          [Line, [Ch]]);
        {runaway_quote, Line} ->
            io_lib:format("~w: runaway quote", [Line]);
        {file, Error} ->
            file:format_error(Error)
    end.

return([], RsAcc) ->
    {ok, lists:reverse(RsAcc)};
return(FsAcc, RsAcc) ->
    {ok, lists:reverse([lists:reverse(FsAcc) | RsAcc])}.

brev(Acc) ->
    unicode:characters_to_binary(lists:reverse(Acc)).
