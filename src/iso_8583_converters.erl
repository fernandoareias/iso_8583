-module(iso_8583_converters).

%%
%% This module is a facade that exposes the main converter APIs.
%% It delegates to specialized converter modules for better maintainability.
%%

%%
%% Include files
%%
-include("iso_8583_types.hrl").

%%
%% Exported Functions
%%
-export([utf8_to_ascii_hex/1, ascii_hex_to_utf8/1, binary_to_ascii_hex/1,
         binary_list_to_ascii_hex/1, ascii_hex_to_binary/1, ascii_hex_to_binary_list/1,
         integer_to_ascii_hex/1, ascii_hex_to_integer/1, numeric_utf8_to_integer/1,
         integer_to_numeric_utf8/1, numeric_utf8_to_bcd/1, bcd_to_numeric_utf8/1, integer_to_bcd/1,
         integer_to_bcd/2, bcd_to_integer/1, ascii_hex_to_bcd/2, bcd_to_ascii_hex/3,
         track2_to_string/2, string_to_track2/1, ascii_to_ebcdic/1, ebcdic_to_ascii/1,
         list_to_bitmap/2, bitmap_to_list/2, pad_with_trailing_spaces/2, strip_leading_zeroes/1,
         strip_trailing_spaces/1, strip_leading_spaces/1, integer_to_string/2]).

                                   % Hex conversion functions

    % Numeric conversions

    % Track 2 conversions

    % EBCDIC / ASCII conversion

    % Bitmap conversion

    % String utilities

%%
%% Hex Conversions (delegated to iso_8583_hex_converter)
%%

utf8_to_ascii_hex(Str) ->
    iso_8583_hex_converter:utf8_to_ascii_hex(Str).

ascii_hex_to_utf8(HexStr) ->
    iso_8583_hex_converter:ascii_hex_to_utf8(HexStr).

binary_to_ascii_hex(BinValue) ->
    iso_8583_hex_converter:binary_to_ascii_hex(BinValue).

binary_list_to_ascii_hex(BinList) ->
    iso_8583_hex_converter:binary_list_to_ascii_hex(BinList).

ascii_hex_to_binary(HexStr) ->
    iso_8583_hex_converter:ascii_hex_to_binary(HexStr).

ascii_hex_to_binary_list(HexStr) ->
    iso_8583_hex_converter:ascii_hex_to_binary_list(HexStr).

integer_to_ascii_hex(IntValue) ->
    iso_8583_hex_converter:integer_to_ascii_hex(IntValue).

ascii_hex_to_integer(AsciiHex) ->
    iso_8583_hex_converter:ascii_hex_to_integer(AsciiHex).

%%
%% Numeric Conversions (delegated to iso_8583_numeric_converter)
%%

numeric_utf8_to_integer(IntStr) ->
    iso_8583_numeric_converter:numeric_utf8_to_integer(IntStr).

integer_to_numeric_utf8(IntValue) ->
    iso_8583_numeric_converter:integer_to_numeric_utf8(IntValue).

numeric_utf8_to_bcd(IntStr) ->
    iso_8583_numeric_converter:numeric_utf8_to_bcd(IntStr).

bcd_to_numeric_utf8(Bcd) ->
    iso_8583_numeric_converter:bcd_to_numeric_utf8(Bcd).

integer_to_bcd(IntValue) ->
    iso_8583_numeric_converter:integer_to_bcd(IntValue).

bcd_to_integer(BcdList) ->
    iso_8583_numeric_converter:bcd_to_integer(BcdList).

%%
%% Track 2 Conversions (delegated to iso_8583_track2_converter)
%%

track2_to_string(Track2Data, Length) ->
    iso_8583_track2_converter:track2_to_string(Track2Data, Length).

string_to_track2(Str) ->
    iso_8583_track2_converter:string_to_track2(Str).

%%
%% EBCDIC Conversions (delegated to iso_8583_ebcdic_converter)
%%

ascii_to_ebcdic(Str) ->
    iso_8583_ebcdic_converter:ascii_to_ebcdic(Str).

ebcdic_to_ascii(EbcdicStr) ->
    iso_8583_ebcdic_converter:ebcdic_to_ascii(EbcdicStr).

%%
%% Bitmap Conversions (delegated to iso_8583_bitmap_converter)
%%

list_to_bitmap(FieldIds, Offset) ->
    iso_8583_bitmap_converter:list_to_bitmap(FieldIds, Offset).

bitmap_to_list(Bitmap, Offset) ->
    iso_8583_bitmap_converter:bitmap_to_list(Bitmap, Offset).

%%
%% String Utilities (delegated to iso_8583_string_utils)
%%

pad_with_trailing_spaces(Str, Length) ->
    iso_8583_string_utils:pad_with_trailing_spaces(Str, Length).

strip_leading_zeroes(Str) ->
    iso_8583_string_utils:strip_leading_zeroes(Str).

strip_trailing_spaces(Str) ->
    iso_8583_string_utils:strip_trailing_spaces(Str).

strip_leading_spaces(Str) ->
    iso_8583_string_utils:strip_leading_spaces(Str).

%% @doc Converts an integer to a string with zero padding.
%%
%% @spec integer_to_string(integer(), integer()) -> string()
integer_to_string(Int, Length) ->
    IntStr = integer_to_list(Int),
    Padding = Length - length(IntStr),
    if Padding > 0 ->
           lists:duplicate(Padding, $0) ++ IntStr;
       true ->
           IntStr
    end.

%% @doc Converts an integer to BCD with specific length (in digits).
%%
%% @spec integer_to_bcd(integer(), integer()) -> list(byte())
integer_to_bcd(IntValue, Length) ->
    PaddedStr = integer_to_string(IntValue, Length),
    ascii_hex_to_bcd(PaddedStr, "0").

%% @doc Converts an ASCII hex string to BCD with padding.
%%
%% @spec ascii_hex_to_bcd(string(), string()) -> list(byte())
ascii_hex_to_bcd(AsciiHex, PadChar) ->
    PaddedAsciiHex =
        case length(AsciiHex) rem 2 of
            0 ->
                AsciiHex;
            1 ->
                PadChar ++ AsciiHex
        end,
    ascii_hex_to_bcd_pairs(PaddedAsciiHex, []).

ascii_hex_to_bcd_pairs([], Result) ->
    lists:reverse(Result);
ascii_hex_to_bcd_pairs([High, Low | Rest], Result) ->
    Byte = (High - $0) * 16 + (Low - $0),
    ascii_hex_to_bcd_pairs(Rest, [Byte | Result]).

%% @doc Converts BCD to ASCII hex string with specific length and padding.
%%
%% @spec bcd_to_ascii_hex(list(byte()), integer(), string()) -> string()
bcd_to_ascii_hex(BcdValue, Length, PadChar) ->
    AsciiHex = bcd_to_ascii_hex_full(BcdValue, []),
    case length(AsciiHex) > Length of
        true ->
            % Strip leading padding
            case AsciiHex of
                [P | Rest] when [P] =:= PadChar ->
                    Rest;
                _ ->
                    AsciiHex
            end;
        false ->
            AsciiHex
    end.

bcd_to_ascii_hex_full([], Result) ->
    lists:flatten(
        lists:reverse(Result));
bcd_to_ascii_hex_full([Byte | Rest], Result) ->
    High = Byte div 16 + $0,
    Low = Byte rem 16 + $0,
    bcd_to_ascii_hex_full(Rest, [[High, Low] | Result]).
