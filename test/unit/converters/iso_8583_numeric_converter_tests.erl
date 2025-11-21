-module(iso_8583_numeric_converter_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Module Exports
%%====================================================================

module_exports_test_() ->
    [?_assert(erlang:function_exported(iso_8583_numeric_converter,
                                       numeric_utf8_to_integer,
                                       1)),
     ?_assert(erlang:function_exported(iso_8583_numeric_converter,
                                       integer_to_numeric_utf8,
                                       1)),
     ?_assert(erlang:function_exported(iso_8583_numeric_converter, numeric_utf8_to_bcd, 1)),
     ?_assert(erlang:function_exported(iso_8583_numeric_converter, bcd_to_numeric_utf8, 1)),
     ?_assert(erlang:function_exported(iso_8583_numeric_converter, integer_to_bcd, 1)),
     ?_assert(erlang:function_exported(iso_8583_numeric_converter, bcd_to_integer, 1))].

%%====================================================================
%% Test integer_to_numeric_utf8/1
%%====================================================================

integer_to_numeric_utf8_test_() ->
    [?_assertEqual(<<"0">>, iso_8583_numeric_converter:integer_to_numeric_utf8(0)),
     ?_assertEqual(<<"1">>, iso_8583_numeric_converter:integer_to_numeric_utf8(1)),
     ?_assertEqual(<<"123">>, iso_8583_numeric_converter:integer_to_numeric_utf8(123)),
     ?_assertEqual(<<"123456">>, iso_8583_numeric_converter:integer_to_numeric_utf8(123456)),
     ?_assertEqual(<<"1000">>, iso_8583_numeric_converter:integer_to_numeric_utf8(1000))].

%%====================================================================
%% Test numeric_utf8_to_integer/1
%%====================================================================

numeric_utf8_to_integer_test_() ->
    [?_assertEqual(0, iso_8583_numeric_converter:numeric_utf8_to_integer(<<"0">>)),
     ?_assertEqual(1, iso_8583_numeric_converter:numeric_utf8_to_integer(<<"1">>)),
     ?_assertEqual(123, iso_8583_numeric_converter:numeric_utf8_to_integer(<<"123">>)),
     ?_assertEqual(123456, iso_8583_numeric_converter:numeric_utf8_to_integer(<<"123456">>)),
     ?_assertEqual(1000, iso_8583_numeric_converter:numeric_utf8_to_integer(<<"1000">>))].

%%====================================================================
%% Test integer_to_bcd/1
%%====================================================================

integer_to_bcd_test_() ->
    [% Note: integer_to_bcd(0) returns []
     ?_assertEqual([], iso_8583_numeric_converter:integer_to_bcd(0)),
     ?_assertEqual([1], iso_8583_numeric_converter:integer_to_bcd(1)),
     ?_assertEqual([18], iso_8583_numeric_converter:integer_to_bcd(12)),  % 0x12
     ?_assertEqual([18, 52, 86],
                   iso_8583_numeric_converter:integer_to_bcd(123456)),  % 0x12, 0x34, 0x56
     ?_assertEqual([16, 0], iso_8583_numeric_converter:integer_to_bcd(1000))].  % 0x10, 0x00

%%====================================================================
%% Test bcd_to_integer/1
%%====================================================================

bcd_to_integer_test_() ->
    [?_assertEqual(0, iso_8583_numeric_converter:bcd_to_integer([])),
     ?_assertEqual(1, iso_8583_numeric_converter:bcd_to_integer([1])),
     ?_assertEqual(12, iso_8583_numeric_converter:bcd_to_integer([18])),  % 0x12
     ?_assertEqual(123456,
                   iso_8583_numeric_converter:bcd_to_integer([18, 52, 86])),  % 0x12, 0x34, 0x56
     ?_assertEqual(1000, iso_8583_numeric_converter:bcd_to_integer([16, 0]))].  % 0x10, 0x00

%%====================================================================
%% Test numeric_utf8_to_bcd/1
%%====================================================================

numeric_utf8_to_bcd_test_() ->
    [?_assertEqual([18, 52, 86],
                   iso_8583_numeric_converter:numeric_utf8_to_bcd(<<"123456">>)),
     ?_assertEqual([16, 0], iso_8583_numeric_converter:numeric_utf8_to_bcd(<<"1000">>)),
     ?_assertEqual([1], iso_8583_numeric_converter:numeric_utf8_to_bcd(<<"1">>))].

%%====================================================================
%% Test bcd_to_numeric_utf8/1
%%====================================================================

bcd_to_numeric_utf8_test_() ->
    [?_assertEqual(<<"0">>, iso_8583_numeric_converter:bcd_to_numeric_utf8([])),
     ?_assertEqual(<<"123456">>, iso_8583_numeric_converter:bcd_to_numeric_utf8([18, 52, 86])),
     ?_assertEqual(<<"1000">>, iso_8583_numeric_converter:bcd_to_numeric_utf8([16, 0])),
     ?_assertEqual(<<"1">>, iso_8583_numeric_converter:bcd_to_numeric_utf8([1]))].

%%====================================================================
%% Test Roundtrip Conversions
%%====================================================================

roundtrip_utf8_integer_test_() ->
    [?_test(begin
                Original = 123456,
                Utf8 = iso_8583_numeric_converter:integer_to_numeric_utf8(Original),
                Result = iso_8583_numeric_converter:numeric_utf8_to_integer(Utf8),
                ?assertEqual(Original, Result)
            end),
     ?_test(begin
                Original = <<"999999">>,
                Int = iso_8583_numeric_converter:numeric_utf8_to_integer(Original),
                Result = iso_8583_numeric_converter:integer_to_numeric_utf8(Int),
                ?assertEqual(Original, Result)
            end)].

roundtrip_bcd_integer_test_() ->
    [?_test(begin
                Original = 123456,
                Bcd = iso_8583_numeric_converter:integer_to_bcd(Original),
                Result = iso_8583_numeric_converter:bcd_to_integer(Bcd),
                ?assertEqual(Original, Result)
            end),
     ?_test(begin
                Original = [18, 52, 86],  % 123456
                Int = iso_8583_numeric_converter:bcd_to_integer(Original),
                Result = iso_8583_numeric_converter:integer_to_bcd(Int),
                ?assertEqual(Original, Result)
            end)].

roundtrip_utf8_bcd_test_() ->
    [?_test(begin
                Original = <<"123456">>,
                Bcd = iso_8583_numeric_converter:numeric_utf8_to_bcd(Original),
                Result = iso_8583_numeric_converter:bcd_to_numeric_utf8(Bcd),
                ?assertEqual(Original, Result)
            end)].

%%====================================================================
%% Test ISO 8583 Common Values
%%====================================================================

iso8583_values_test_() ->
    [% Processing code
     ?_test(begin
                Bcd = iso_8583_numeric_converter:integer_to_bcd(0),
                ?assertEqual([], Bcd)
            end),
     % Amount 1000 cents
     ?_test(begin
                Bcd = iso_8583_numeric_converter:integer_to_bcd(1000),
                ?assertEqual([16, 0], Bcd)
            end),
     % STAN 123456
     ?_test(begin
                Bcd = iso_8583_numeric_converter:integer_to_bcd(123456),
                ?assertEqual([18, 52, 86], Bcd)
            end)].
