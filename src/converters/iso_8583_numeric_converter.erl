-module(iso_8583_numeric_converter).

%%
%% Include files
%%
-include("iso_8583_types.hrl").

%%
%% Exported Functions
%%
-export([numeric_utf8_to_integer/1, integer_to_numeric_utf8/1, numeric_utf8_to_bcd/1,
         bcd_to_numeric_utf8/1, integer_to_bcd/1, bcd_to_integer/1]).    % Numeric UTF8 conversions

    % BCD conversions

%%
%% API Functions
%%

%% @doc Converts an integer to its UTF8 decimal string representation.
%%
%% @spec integer_to_numeric_utf8(integer()) -> utf8()
-spec integer_to_numeric_utf8(integer()) -> utf8().
integer_to_numeric_utf8(IntValue) ->
    unicode:characters_to_binary(integer_to_list(IntValue), utf8).

%% @doc Converts a UTF8 string encoding a decimal value to its integer value.
%%
%% @spec numeric_utf8_to_integer(utf8()) -> integer()
-spec numeric_utf8_to_integer(utf8()) -> integer().
numeric_utf8_to_integer(IntStr) ->
    list_to_integer(binary_to_list(IntStr)).

%% @doc Converts a UTF8 string encoding a decimal value to a BCD encoding.
%%
%% @spec numeric_utf8_to_bcd(utf8()) -> bcd()
-spec numeric_utf8_to_bcd(utf8()) -> bcd().
numeric_utf8_to_bcd(IntStr) ->
    integer_to_bcd(numeric_utf8_to_integer(IntStr)).

%% @doc Converts a BCD encoding to a UTF8 string encoding a decimal value.
%%
%% @spec bcd_to_numeric_utf8(bcd()) -> utf8()
-spec bcd_to_numeric_utf8(bcd()) -> utf8().
bcd_to_numeric_utf8(Bcd) ->
    integer_to_numeric_utf8(bcd_to_integer(Bcd)).

%% @doc Converts a list of BCD encoded bytes to an integer.
%%
%% @spec bcd_to_integer(bcd()) -> integer()
-spec bcd_to_integer(bcd()) -> integer().
bcd_to_integer(BcdList) ->
    F = fun(Value, Acc) ->
           Dig1 = Value div 16,
           Dig2 = Value rem 16,
           100 * Acc + 10 * Dig1 + Dig2
        end,
    lists:foldl(F, 0, BcdList).

%% @doc Converts an integer to a list of BCD encoded bytes.
%%
%% @spec integer_to_bcd(integer()) -> bcd()
-spec integer_to_bcd(integer()) -> bcd().
integer_to_bcd(IntValue) ->
    integer_to_bcd(IntValue, []).

%%
%% Local Functions
%%

integer_to_bcd(0, Result) ->
    Result;
integer_to_bcd(IntValue, Result) ->
    Byte = IntValue rem 100,
    integer_to_bcd(IntValue div 100, [Byte div 10 * 16 + Byte rem 10 | Result]).
