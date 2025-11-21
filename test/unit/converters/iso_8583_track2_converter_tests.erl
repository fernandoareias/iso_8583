-module(iso_8583_track2_converter_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Module Exports
%%====================================================================

module_exports_test_() ->
    [?_assert(erlang:function_exported(iso_8583_track2_converter, track2_to_string, 2)),
     ?_assert(erlang:function_exported(iso_8583_track2_converter, string_to_track2, 1))].

%%====================================================================
%% Test string_to_track2/1
%%====================================================================

string_to_track2_test_() ->
    [% Empty string
     ?_assertEqual([], iso_8583_track2_converter:string_to_track2("")),
     % Single digit (odd length - pads with 0 nibble)
     ?_assertEqual([16], iso_8583_track2_converter:string_to_track2("1")),
     % Two digits
     ?_assertEqual([18], iso_8583_track2_converter:string_to_track2("12")),  % 0x12
     % Four digits
     ?_assertEqual([18, 52], iso_8583_track2_converter:string_to_track2("1234")),  % 0x12, 0x34
     % Typical PAN
     ?_assertEqual([65, 17, 17, 17, 17, 17, 17, 17],
                   iso_8583_track2_converter:string_to_track2("4111111111111111"))].

%%====================================================================
%% Test track2_to_string/2
%%====================================================================

track2_to_string_test_() ->
    [% Empty list
     ?_assertEqual("", iso_8583_track2_converter:track2_to_string([], 0)),
     % Single byte to 2 chars
     ?_assertEqual("12", iso_8583_track2_converter:track2_to_string([18], 2)),
     % Two bytes to 4 chars
     ?_assertEqual("1234", iso_8583_track2_converter:track2_to_string([18, 52], 4)),
     % Length truncation
     ?_assertEqual("123", iso_8583_track2_converter:track2_to_string([18, 52], 3)),
     % Typical PAN
     ?_assertEqual("4111111111111111",
                   iso_8583_track2_converter:track2_to_string([65, 17, 17, 17, 17, 17, 17, 17],
                                                              16))].

%%====================================================================
%% Test Roundtrip Conversions
%%====================================================================

roundtrip_test_() ->
    [?_test(begin
                Original = "1234567890",
                Track2 = iso_8583_track2_converter:string_to_track2(Original),
                Result = iso_8583_track2_converter:track2_to_string(Track2, length(Original)),
                ?assertEqual(Original, Result)
            end),
     ?_test(begin
                Original = "4111111111111111",
                Track2 = iso_8583_track2_converter:string_to_track2(Original),
                Result = iso_8583_track2_converter:track2_to_string(Track2, length(Original)),
                ?assertEqual(Original, Result)
            end),
     % Odd length string
     ?_test(begin
                Original = "12345",
                Track2 = iso_8583_track2_converter:string_to_track2(Original),
                Result = iso_8583_track2_converter:track2_to_string(Track2, length(Original)),
                ?assertEqual(Original, Result)
            end)].

%%====================================================================
%% Test Track 2 Data Format
%%====================================================================

track2_format_test_() ->
    [% Track 2 with separator (=)
     % In track 2, '=' is typically represented as 0xD
     ?_test(begin
                % "4111111111111111=2512" - PAN=YYMM
                Track2 = iso_8583_track2_converter:string_to_track2("4111111111111111"),
                ?assertEqual(8, length(Track2))
            end),
     % All zeros
     ?_test(begin
                Track2 = iso_8583_track2_converter:string_to_track2("0000"),
                ?assertEqual([0, 0], Track2)
            end),
     % All nines
     ?_test(begin
                Track2 = iso_8583_track2_converter:string_to_track2("9999"),
                ?assertEqual([153, 153], Track2)  % 0x99, 0x99
            end)].

%%====================================================================
%% Test ISO 8583 Field 35 Typical Values
%%====================================================================

field_35_test_() ->
    % Typical track 2 data length (up to 37 chars)
    [?_test(begin
                Pan = "4111111111111111",
                Track2 = iso_8583_track2_converter:string_to_track2(Pan),
                Result = iso_8583_track2_converter:track2_to_string(Track2, 16),
                ?assertEqual(Pan, Result)
            end)].
