-module(iso_8583_string_utils).

%%
%% Include files
%%
-include("iso_8583_types.hrl").

%%
%% Exported Functions
%%
-export([pad_with_trailing_spaces/2, strip_leading_zeroes/1, strip_trailing_spaces/1,
         strip_leading_spaces/1]).

%%
%% API Functions
%%

%% @doc Pads a UTF8 string with a number of spaces so that the
%%      resultant string has specified length.
%%
%% @spec pad_with_trailing_spaces(utf8(), integer()) -> utf8()
-spec pad_with_trailing_spaces(utf8(), integer()) -> utf8().
pad_with_trailing_spaces(Str, Length) when size(Str) =:= Length ->
    Str;
pad_with_trailing_spaces(Str, Length) when size(Str) < Length ->
    pad_with_trailing_spaces(<<Str/binary, " ">>, Length).

%% @doc Strips trailing spaces from an ASCII string.
%%
%% @spec strip_trailing_spaces(string()) -> string()
-spec strip_trailing_spaces(string()) -> string().
strip_trailing_spaces(Str) ->
    lists:reverse(strip_leading_spaces(lists:reverse(Str))).

%% @doc Strips leading zeroes from an ASCII string.
%%
%% @spec strip_leading_zeroes(string()) -> string()
-spec strip_leading_zeroes(string()) -> string().
strip_leading_zeroes([$0 | Tail]) ->
    strip_leading_zeroes(Tail);
strip_leading_zeroes(Str) ->
    Str.

%% @doc Strips leading spaces from an ASCII string.
%%
%% @spec strip_leading_spaces(string()) -> string()
-spec strip_leading_spaces(string()) -> string().
strip_leading_spaces([$  | Tail]) ->
    strip_leading_spaces(Tail);
strip_leading_spaces(Str) ->
    Str.
