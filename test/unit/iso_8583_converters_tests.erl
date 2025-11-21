-module(iso_8583_converters_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Hex Conversion Tests
%%====================================================================

utf8_to_ascii_hex_test() ->
    Result = iso_8583_converters:utf8_to_ascii_hex(<<"HELLO">>),
    ?assertEqual(<<"48454C4C4F">>, Result).

ascii_hex_to_utf8_test() ->
    Result = iso_8583_converters:ascii_hex_to_utf8(<<"48454C4C4F">>),
    ?assertEqual(<<"HELLO">>, Result).

binary_to_ascii_hex_test() ->
    Result = iso_8583_converters:binary_to_ascii_hex(<<"HELLO">>),
    ?assertEqual(<<"48454C4C4F">>, Result).

binary_list_to_ascii_hex_test() ->
    Result = iso_8583_converters:binary_list_to_ascii_hex([72, 69, 76, 76, 79]),
    ?assertEqual(<<"48454C4C4F">>, Result).

ascii_hex_to_binary_test() ->
    Result = iso_8583_converters:ascii_hex_to_binary(<<"48454C4C4F">>),
    ?assertEqual(<<"HELLO">>, Result).

ascii_hex_to_binary_list_test() ->
    Result = iso_8583_converters:ascii_hex_to_binary_list(<<"48454C4C4F">>),
    ?assertEqual([72, 69, 76, 76, 79], Result).

integer_to_ascii_hex_test() ->
    Result = iso_8583_converters:integer_to_ascii_hex(255),
    ?assertEqual(<<"FF">>, Result).

ascii_hex_to_integer_test() ->
    ?assertEqual(255, iso_8583_converters:ascii_hex_to_integer(<<"FF">>)).

%%====================================================================
%% Numeric Conversion Tests
%%====================================================================

numeric_utf8_to_integer_test() ->
    ?assertEqual(12345, iso_8583_converters:numeric_utf8_to_integer(<<"12345">>)).

integer_to_numeric_utf8_test() ->
    Result = iso_8583_converters:integer_to_numeric_utf8(12345),
    ?assertEqual(<<"12345">>, Result).

numeric_utf8_to_bcd_test() ->
    Result = iso_8583_converters:numeric_utf8_to_bcd(<<"1234">>),
    ?assert(is_list(Result) orelse is_binary(Result)).

bcd_to_numeric_utf8_test() ->
    Bcd = iso_8583_converters:numeric_utf8_to_bcd(<<"1234">>),
    Result = iso_8583_converters:bcd_to_numeric_utf8(Bcd),
    ?assertEqual(<<"1234">>, Result).

integer_to_bcd_test() ->
    Result = iso_8583_converters:integer_to_bcd(1234),
    ?assert(is_list(Result)).

bcd_to_integer_test() ->
    Bcd = iso_8583_converters:integer_to_bcd(1234),
    ?assertEqual(1234, iso_8583_converters:bcd_to_integer(Bcd)).

integer_to_bcd_with_length_test() ->
    Result = iso_8583_converters:integer_to_bcd(42, 4),
    ?assert(is_list(Result)).

ascii_hex_to_bcd_even_test() ->
    Result = iso_8583_converters:ascii_hex_to_bcd("1234", "0"),
    ?assertEqual([16#12, 16#34], Result).

ascii_hex_to_bcd_odd_test() ->
    Result = iso_8583_converters:ascii_hex_to_bcd("123", "0"),
    ?assertEqual([16#01, 16#23], Result).

bcd_to_ascii_hex_test() ->
    Result = iso_8583_converters:bcd_to_ascii_hex([16#12, 16#34], 4, "0"),
    ?assertEqual("1234", Result).

bcd_to_ascii_hex_strip_padding_test() ->
    Result = iso_8583_converters:bcd_to_ascii_hex([16#01, 16#23], 3, "0"),
    ?assertEqual("123", Result).

bcd_to_ascii_hex_no_strip_when_different_pad_test() ->
    % When length(AsciiHex) > Length but first char != PadChar, return AsciiHex unchanged
    Result = iso_8583_converters:bcd_to_ascii_hex([16#12, 16#34], 3, "0"),
    ?assertEqual("1234", Result).

%%====================================================================
%% Track 2 Conversion Tests
%%====================================================================

track2_to_string_test() ->
    Result = iso_8583_converters:track2_to_string([16#45, 16#67], 4),
    ?assert(is_list(Result)).

string_to_track2_test() ->
    Result = iso_8583_converters:string_to_track2("4567"),
    ?assert(is_list(Result)).

%%====================================================================
%% EBCDIC Conversion Tests
%%====================================================================

ascii_to_ebcdic_test() ->
    Result = iso_8583_converters:ascii_to_ebcdic("A"),
    ?assert(is_list(Result) orelse is_binary(Result)).

ebcdic_to_ascii_test() ->
    Ebcdic = iso_8583_converters:ascii_to_ebcdic("ABC"),
    Result = iso_8583_converters:ebcdic_to_ascii(Ebcdic),
    ?assertEqual("ABC", Result).

%%====================================================================
%% Bitmap Conversion Tests
%%====================================================================

list_to_bitmap_test() ->
    Result = iso_8583_converters:list_to_bitmap([2, 3, 4], 0),
    ?assert(is_list(Result) orelse is_binary(Result)).

bitmap_to_list_test() ->
    Bitmap = iso_8583_converters:list_to_bitmap([2, 3, 4], 0),
    Result = iso_8583_converters:bitmap_to_list(Bitmap, 0),
    ?assert(lists:member(2, Result)),
    ?assert(lists:member(3, Result)),
    ?assert(lists:member(4, Result)).

%%====================================================================
%% String Utility Tests
%%====================================================================

pad_with_trailing_spaces_test() ->
    ?assertEqual(<<"ABC   ">>, iso_8583_converters:pad_with_trailing_spaces(<<"ABC">>, 6)).

strip_leading_zeroes_test() ->
    ?assertEqual("123", iso_8583_converters:strip_leading_zeroes("000123")).

strip_trailing_spaces_test() ->
    ?assertEqual("ABC", iso_8583_converters:strip_trailing_spaces("ABC   ")).

strip_leading_spaces_test() ->
    ?assertEqual("ABC", iso_8583_converters:strip_leading_spaces("   ABC")).

integer_to_string_test() ->
    ?assertEqual("0042", iso_8583_converters:integer_to_string(42, 4)).

integer_to_string_no_padding_test() ->
    ?assertEqual("12345", iso_8583_converters:integer_to_string(12345, 4)).

%%====================================================================
%% Module Exports Tests
%%====================================================================

module_exports_test_() ->
    [?_assert(erlang:function_exported(iso_8583_converters, utf8_to_ascii_hex, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, ascii_hex_to_utf8, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, binary_to_ascii_hex, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, binary_list_to_ascii_hex, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, ascii_hex_to_binary, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, ascii_hex_to_binary_list, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, integer_to_ascii_hex, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, ascii_hex_to_integer, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, numeric_utf8_to_integer, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, integer_to_numeric_utf8, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, numeric_utf8_to_bcd, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, bcd_to_numeric_utf8, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, integer_to_bcd, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, integer_to_bcd, 2)),
     ?_assert(erlang:function_exported(iso_8583_converters, bcd_to_integer, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, ascii_hex_to_bcd, 2)),
     ?_assert(erlang:function_exported(iso_8583_converters, bcd_to_ascii_hex, 3)),
     ?_assert(erlang:function_exported(iso_8583_converters, track2_to_string, 2)),
     ?_assert(erlang:function_exported(iso_8583_converters, string_to_track2, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, ascii_to_ebcdic, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, ebcdic_to_ascii, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, list_to_bitmap, 2)),
     ?_assert(erlang:function_exported(iso_8583_converters, bitmap_to_list, 2)),
     ?_assert(erlang:function_exported(iso_8583_converters, pad_with_trailing_spaces, 2)),
     ?_assert(erlang:function_exported(iso_8583_converters, strip_leading_zeroes, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, strip_trailing_spaces, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, strip_leading_spaces, 1)),
     ?_assert(erlang:function_exported(iso_8583_converters, integer_to_string, 2))].
