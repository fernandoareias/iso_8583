-module(iso_8583_hex_converter).

%%
%% Include files
%%
-include("iso_8583_types.hrl").

%%
%% Exported Functions
%%
-export([utf8_to_ascii_hex/1, ascii_hex_to_utf8/1, binary_to_ascii_hex/1,
         binary_list_to_ascii_hex/1, ascii_hex_to_binary/1, ascii_hex_to_binary_list/1,
         integer_to_ascii_hex/1, ascii_hex_to_integer/1]).

                                                              % UTF8 to/from ASCII hex

    % Binary to/from ASCII hex

    % Integer to/from ASCII hex

%%
%% API Functions
%%

%% @doc Converts a UTF8 string of characters to a string containing
%%      the ASCII hex character codes.
%%
%% @spec utf8_to_ascii_hex(utf8()) -> utf8()
-spec utf8_to_ascii_hex(utf8()) -> utf8().
utf8_to_ascii_hex(Str) ->
    utf8_to_ascii_hex(Str, <<>>).

%% @doc Converts a string containing ASCII hex characters
%%      to an equivalent UTF8 string().
%%
%% @spec ascii_hex_to_utf8(utf8()) -> utf8()
-spec ascii_hex_to_utf8(utf8()) -> utf8().
ascii_hex_to_utf8(HexStr) ->
    ascii_hex_to_utf8(HexStr, <<>>).

%% @doc Returns the ASCII hex encoding of a binary value.
%%
%% @spec binary_to_ascii_hex(binary()) -> utf8()
-spec binary_to_ascii_hex(binary()) -> utf8().
binary_to_ascii_hex(BinValue) ->
    binary_to_ascii_hex(BinValue, <<>>).

%% @doc Returns the ASCII hex encoding of a list of bytes.
%%
%% @spec binary_list_to_ascii_hex(list(byte())) -> utf8()
-spec binary_list_to_ascii_hex([byte()]) -> utf8().
binary_list_to_ascii_hex(BinList) ->
    binary_to_ascii_hex(list_to_binary(BinList), <<>>).

%% @doc Returns the binary value corresponding to an ASCII hex string.
%%
%% @spec ascii_hex_to_binary(utf8()) -> binary()
-spec ascii_hex_to_binary(utf8()) -> binary().
ascii_hex_to_binary(HexStr) ->
    list_to_binary(ascii_hex_to_binary_list(HexStr)).

%% @doc Returns a binary list corresponding to an ASCII hex string.
%%
%% @spec ascii_hex_to_binary_list(utf8()) -> bcd()
-spec ascii_hex_to_binary_list(utf8()) -> bcd().
ascii_hex_to_binary_list(HexStr) ->
    case size(HexStr) rem 2 of
        0 ->
            ascii_hex_to_bytes(HexStr, []);
        1 ->
            ascii_hex_to_bytes(<<$0, HexStr/binary>>, [])
    end.

%% @doc Converts an integer to ASCII hex string.
%%
%% @spec integer_to_ascii_hex(integer()) -> utf8()
-spec integer_to_ascii_hex(integer()) -> utf8().
integer_to_ascii_hex(IntValue) ->
    list_to_binary(integer_to_list(IntValue, 16)).

%% @doc Converts an ASCII hex string to integer.
%%
%% @spec ascii_hex_to_integer(utf8()) -> integer()
-spec ascii_hex_to_integer(utf8()) -> integer().
ascii_hex_to_integer(AsciiHex) ->
    list_to_integer(binary_to_list(AsciiHex), 16).

%%
%% Local Functions
%%

utf8_to_ascii_hex(<<>>, Result) ->
    Result;
utf8_to_ascii_hex(<<Char, Tail/binary>>, Result) ->
    Msb = digit_to_ascii_hex(Char div 16),
    Lsb = digit_to_ascii_hex(Char rem 16),
    utf8_to_ascii_hex(Tail, <<Result/binary, Msb, Lsb>>).

ascii_hex_to_utf8(<<>>, Result) ->
    Result;
ascii_hex_to_utf8(<<Dig1, Dig2, Tail/binary>>, Result) ->
    Char = ascii_hex_to_digit([Dig1]) * 16 + ascii_hex_to_digit([Dig2]),
    ascii_hex_to_utf8(Tail, <<Result/binary, Char>>).

binary_to_ascii_hex(<<>>, Result) ->
    Result;
binary_to_ascii_hex(<<H, T/binary>>, Result) ->
    Msn = digit_to_ascii_hex(H div 16),
    Lsn = digit_to_ascii_hex(H rem 16),
    binary_to_ascii_hex(T, <<Result/binary, Msn, Lsn>>).

ascii_hex_to_bytes(<<>>, Result) ->
    lists:reverse(Result);
ascii_hex_to_bytes(<<Msd, Lsd, Tail/binary>>, Result) ->
    Msn = ascii_hex_to_digit([Msd]),
    Lsn = ascii_hex_to_digit([Lsd]),
    ascii_hex_to_bytes(Tail, [Msn * 16 + Lsn | Result]).

ascii_hex_to_digit([A]) when A >= $0 andalso A =< $9 ->
    A - $0;
ascii_hex_to_digit([A]) when A >= $A andalso A =< $F ->
    A - 55;
ascii_hex_to_digit([A]) when A >= $a andalso A =< $f ->
    A - 87.

digit_to_ascii_hex(D) when D >= 0 andalso D =< 9 ->
    48 + D;
digit_to_ascii_hex(D) when D >= 10 andalso D =< 15 ->
    55 + D.
