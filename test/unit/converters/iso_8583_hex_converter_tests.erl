-module(iso_8583_hex_converter_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Module Exports
%%====================================================================

module_exports_test_() ->
    [?_assert(erlang:function_exported(iso_8583_hex_converter, utf8_to_ascii_hex, 1)),
     ?_assert(erlang:function_exported(iso_8583_hex_converter, ascii_hex_to_utf8, 1)),
     ?_assert(erlang:function_exported(iso_8583_hex_converter, binary_to_ascii_hex, 1)),
     ?_assert(erlang:function_exported(iso_8583_hex_converter, binary_list_to_ascii_hex, 1)),
     ?_assert(erlang:function_exported(iso_8583_hex_converter, ascii_hex_to_binary, 1)),
     ?_assert(erlang:function_exported(iso_8583_hex_converter, ascii_hex_to_binary_list, 1)),
     ?_assert(erlang:function_exported(iso_8583_hex_converter, integer_to_ascii_hex, 1)),
     ?_assert(erlang:function_exported(iso_8583_hex_converter, ascii_hex_to_integer, 1))].

%%====================================================================
%% Test utf8_to_ascii_hex/1
%%====================================================================

utf8_to_ascii_hex_test_() ->
    [?_assertEqual(<<>>, iso_8583_hex_converter:utf8_to_ascii_hex(<<>>)),
     ?_assertEqual(<<"00">>, iso_8583_hex_converter:utf8_to_ascii_hex(<<0>>)),
     ?_assertEqual(<<"FF">>, iso_8583_hex_converter:utf8_to_ascii_hex(<<255>>)),
     ?_assertEqual(<<"48454C4C4F">>, iso_8583_hex_converter:utf8_to_ascii_hex(<<"HELLO">>)),
     ?_assertEqual(<<"30313233">>, iso_8583_hex_converter:utf8_to_ascii_hex(<<"0123">>))].

%%====================================================================
%% Test ascii_hex_to_utf8/1
%%====================================================================

ascii_hex_to_utf8_test_() ->
    [?_assertEqual(<<>>, iso_8583_hex_converter:ascii_hex_to_utf8(<<>>)),
     ?_assertEqual(<<0>>, iso_8583_hex_converter:ascii_hex_to_utf8(<<"00">>)),
     ?_assertEqual(<<255>>, iso_8583_hex_converter:ascii_hex_to_utf8(<<"FF">>)),
     ?_assertEqual(<<"HELLO">>, iso_8583_hex_converter:ascii_hex_to_utf8(<<"48454C4C4F">>)),
     % Lowercase hex
     ?_assertEqual(<<255>>, iso_8583_hex_converter:ascii_hex_to_utf8(<<"ff">>))].

%%====================================================================
%% Test binary_to_ascii_hex/1
%%====================================================================

binary_to_ascii_hex_test_() ->
    [?_assertEqual(<<>>, iso_8583_hex_converter:binary_to_ascii_hex(<<>>)),
     ?_assertEqual(<<"0102030405060708">>,
                   iso_8583_hex_converter:binary_to_ascii_hex(<<1, 2, 3, 4, 5, 6, 7, 8>>)),
     ?_assertEqual(<<"FFFFFFFFFFFFFFFF">>,
                   iso_8583_hex_converter:binary_to_ascii_hex(<<255,
                                                                255,
                                                                255,
                                                                255,
                                                                255,
                                                                255,
                                                                255,
                                                                255>>)),
     ?_assertEqual(<<"0000000000000000">>,
                   iso_8583_hex_converter:binary_to_ascii_hex(<<0, 0, 0, 0, 0, 0, 0, 0>>))].

%%====================================================================
%% Test binary_list_to_ascii_hex/1
%%====================================================================

binary_list_to_ascii_hex_test_() ->
    [?_assertEqual(<<>>, iso_8583_hex_converter:binary_list_to_ascii_hex([])),
     ?_assertEqual(<<"0102030405060708">>,
                   iso_8583_hex_converter:binary_list_to_ascii_hex([1, 2, 3, 4, 5, 6, 7, 8])),
     ?_assertEqual(<<"FF">>, iso_8583_hex_converter:binary_list_to_ascii_hex([255])),
     ?_assertEqual(<<"00">>, iso_8583_hex_converter:binary_list_to_ascii_hex([0]))].

%%====================================================================
%% Test ascii_hex_to_binary/1
%%====================================================================

ascii_hex_to_binary_test_() ->
    [?_assertEqual(<<>>, iso_8583_hex_converter:ascii_hex_to_binary(<<>>)),
     ?_assertEqual(<<1, 2, 3, 4, 5, 6, 7, 8>>,
                   iso_8583_hex_converter:ascii_hex_to_binary(<<"0102030405060708">>)),
     ?_assertEqual(<<255, 255, 255, 255>>,
                   iso_8583_hex_converter:ascii_hex_to_binary(<<"FFFFFFFF">>)),
     % Lowercase
     ?_assertEqual(<<171, 205, 239>>,
                   iso_8583_hex_converter:ascii_hex_to_binary(<<"abcdef">>))].

%%====================================================================
%% Test ascii_hex_to_binary_list/1
%%====================================================================

ascii_hex_to_binary_list_test_() ->
    [?_assertEqual([], iso_8583_hex_converter:ascii_hex_to_binary_list(<<>>)),
     ?_assertEqual([1, 2, 3, 4],
                   iso_8583_hex_converter:ascii_hex_to_binary_list(<<"01020304">>)),
     ?_assertEqual([255], iso_8583_hex_converter:ascii_hex_to_binary_list(<<"FF">>)),
     % Odd length - prepends 0
     ?_assertEqual([15], iso_8583_hex_converter:ascii_hex_to_binary_list(<<"F">>)),
     ?_assertEqual([1, 35], iso_8583_hex_converter:ascii_hex_to_binary_list(<<"123">>))].

%%====================================================================
%% Test integer_to_ascii_hex/1
%%====================================================================

integer_to_ascii_hex_test_() ->
    [?_assertEqual(<<"0">>, iso_8583_hex_converter:integer_to_ascii_hex(0)),
     ?_assertEqual(<<"1">>, iso_8583_hex_converter:integer_to_ascii_hex(1)),
     ?_assertEqual(<<"A">>, iso_8583_hex_converter:integer_to_ascii_hex(10)),
     ?_assertEqual(<<"F">>, iso_8583_hex_converter:integer_to_ascii_hex(15)),
     ?_assertEqual(<<"10">>, iso_8583_hex_converter:integer_to_ascii_hex(16)),
     ?_assertEqual(<<"FF">>, iso_8583_hex_converter:integer_to_ascii_hex(255)),
     ?_assertEqual(<<"100">>, iso_8583_hex_converter:integer_to_ascii_hex(256))].

%%====================================================================
%% Test ascii_hex_to_integer/1
%%====================================================================

ascii_hex_to_integer_test_() ->
    [?_assertEqual(0, iso_8583_hex_converter:ascii_hex_to_integer(<<"0">>)),
     ?_assertEqual(1, iso_8583_hex_converter:ascii_hex_to_integer(<<"1">>)),
     ?_assertEqual(10, iso_8583_hex_converter:ascii_hex_to_integer(<<"A">>)),
     ?_assertEqual(10, iso_8583_hex_converter:ascii_hex_to_integer(<<"a">>)),
     ?_assertEqual(15, iso_8583_hex_converter:ascii_hex_to_integer(<<"F">>)),
     ?_assertEqual(255, iso_8583_hex_converter:ascii_hex_to_integer(<<"FF">>)),
     ?_assertEqual(256, iso_8583_hex_converter:ascii_hex_to_integer(<<"100">>))].

%%====================================================================
%% Test Roundtrip Conversions
%%====================================================================

roundtrip_utf8_test_() ->
    [?_test(begin
                Original = <<"HELLO">>,
                Hex = iso_8583_hex_converter:utf8_to_ascii_hex(Original),
                Result = iso_8583_hex_converter:ascii_hex_to_utf8(Hex),
                ?assertEqual(Original, Result)
            end),
     ?_test(begin
                Original = <<"0200">>,
                Hex = iso_8583_hex_converter:utf8_to_ascii_hex(Original),
                Result = iso_8583_hex_converter:ascii_hex_to_utf8(Hex),
                ?assertEqual(Original, Result)
            end)].

roundtrip_binary_test_() ->
    [?_test(begin
                Original = <<1, 2, 3, 4, 5, 6, 7, 8>>,
                Hex = iso_8583_hex_converter:binary_to_ascii_hex(Original),
                Result = iso_8583_hex_converter:ascii_hex_to_binary(Hex),
                ?assertEqual(Original, Result)
            end),
     ?_test(begin
                Original = <<255, 0, 128, 64>>,
                Hex = iso_8583_hex_converter:binary_to_ascii_hex(Original),
                Result = iso_8583_hex_converter:ascii_hex_to_binary(Hex),
                ?assertEqual(Original, Result)
            end)].

roundtrip_integer_test_() ->
    [?_test(begin
                Original = 255,
                Hex = iso_8583_hex_converter:integer_to_ascii_hex(Original),
                Result = iso_8583_hex_converter:ascii_hex_to_integer(Hex),
                ?assertEqual(Original, Result)
            end),
     ?_test(begin
                Original = 65535,
                Hex = iso_8583_hex_converter:integer_to_ascii_hex(Original),
                Result = iso_8583_hex_converter:ascii_hex_to_integer(Hex),
                ?assertEqual(Original, Result)
            end)].
