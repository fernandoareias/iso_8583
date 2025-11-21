-module(iso_8583_ebcdic_converter_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Module Exports
%%====================================================================

module_exports_test_() ->
    [?_assert(erlang:function_exported(iso_8583_ebcdic_converter, ascii_to_ebcdic, 1)),
     ?_assert(erlang:function_exported(iso_8583_ebcdic_converter, ebcdic_to_ascii, 1))].

%%====================================================================
%% Test ascii_to_ebcdic/1 - Digits
%%====================================================================

ascii_to_ebcdic_digits_test_() ->
    [% EBCDIC digits are F0-F9 (240-249)
     ?_assertEqual([240], iso_8583_ebcdic_converter:ascii_to_ebcdic("0")),
     ?_assertEqual([241], iso_8583_ebcdic_converter:ascii_to_ebcdic("1")),
     ?_assertEqual([242], iso_8583_ebcdic_converter:ascii_to_ebcdic("2")),
     ?_assertEqual([243], iso_8583_ebcdic_converter:ascii_to_ebcdic("3")),
     ?_assertEqual([244], iso_8583_ebcdic_converter:ascii_to_ebcdic("4")),
     ?_assertEqual([245], iso_8583_ebcdic_converter:ascii_to_ebcdic("5")),
     ?_assertEqual([246], iso_8583_ebcdic_converter:ascii_to_ebcdic("6")),
     ?_assertEqual([247], iso_8583_ebcdic_converter:ascii_to_ebcdic("7")),
     ?_assertEqual([248], iso_8583_ebcdic_converter:ascii_to_ebcdic("8")),
     ?_assertEqual([249], iso_8583_ebcdic_converter:ascii_to_ebcdic("9")),
     ?_assertEqual([240, 241, 242, 243, 244, 245, 246, 247, 248, 249],
                   iso_8583_ebcdic_converter:ascii_to_ebcdic("0123456789"))].

%%====================================================================
%% Test ascii_to_ebcdic/1 - Uppercase Letters
%%====================================================================

ascii_to_ebcdic_uppercase_test_() ->
    [% A-I: C1-C9 (193-201)
     ?_assertEqual([193], iso_8583_ebcdic_converter:ascii_to_ebcdic("A")),
     ?_assertEqual([194], iso_8583_ebcdic_converter:ascii_to_ebcdic("B")),
     ?_assertEqual([201], iso_8583_ebcdic_converter:ascii_to_ebcdic("I")),
     % J-R: D1-D9 (209-217)
     ?_assertEqual([209], iso_8583_ebcdic_converter:ascii_to_ebcdic("J")),
     ?_assertEqual([210], iso_8583_ebcdic_converter:ascii_to_ebcdic("K")),
     ?_assertEqual([217], iso_8583_ebcdic_converter:ascii_to_ebcdic("R")),
     % S-Z: E2-E9 (226-233)
     ?_assertEqual([226], iso_8583_ebcdic_converter:ascii_to_ebcdic("S")),
     ?_assertEqual([227], iso_8583_ebcdic_converter:ascii_to_ebcdic("T")),
     ?_assertEqual([233], iso_8583_ebcdic_converter:ascii_to_ebcdic("Z"))].

%%====================================================================
%% Test ascii_to_ebcdic/1 - Lowercase Letters
%%====================================================================

ascii_to_ebcdic_lowercase_test_() ->
    [% a-i: 81-89 (129-137)
     ?_assertEqual([129], iso_8583_ebcdic_converter:ascii_to_ebcdic("a")),
     ?_assertEqual([137], iso_8583_ebcdic_converter:ascii_to_ebcdic("i")),
     % j-r: 91-99 (145-153)
     ?_assertEqual([145], iso_8583_ebcdic_converter:ascii_to_ebcdic("j")),
     ?_assertEqual([153], iso_8583_ebcdic_converter:ascii_to_ebcdic("r")),
     % s-z: A2-A9 (162-169)
     ?_assertEqual([162], iso_8583_ebcdic_converter:ascii_to_ebcdic("s")),
     ?_assertEqual([169], iso_8583_ebcdic_converter:ascii_to_ebcdic("z"))].

%%====================================================================
%% Test ascii_to_ebcdic/1 - Special Characters
%%====================================================================

ascii_to_ebcdic_special_test_() ->
    [?_assertEqual([64], iso_8583_ebcdic_converter:ascii_to_ebcdic(" ")),
     ?_assertEqual([75], iso_8583_ebcdic_converter:ascii_to_ebcdic(".")),
     ?_assertEqual([96], iso_8583_ebcdic_converter:ascii_to_ebcdic("-")),
     ?_assertEqual([97], iso_8583_ebcdic_converter:ascii_to_ebcdic("/")),
     ?_assertEqual([107], iso_8583_ebcdic_converter:ascii_to_ebcdic(",")),
     ?_assertEqual([122], iso_8583_ebcdic_converter:ascii_to_ebcdic(":")),
     ?_assertEqual([124], iso_8583_ebcdic_converter:ascii_to_ebcdic("@")),
     ?_assertEqual([126], iso_8583_ebcdic_converter:ascii_to_ebcdic("="))].

%%====================================================================
%% Test ebcdic_to_ascii/1 - Digits
%%====================================================================

ebcdic_to_ascii_digits_test_() ->
    [?_assertEqual("0", iso_8583_ebcdic_converter:ebcdic_to_ascii([240])),
     ?_assertEqual("1", iso_8583_ebcdic_converter:ebcdic_to_ascii([241])),
     ?_assertEqual("9", iso_8583_ebcdic_converter:ebcdic_to_ascii([249])),
     ?_assertEqual("0123456789",
                   iso_8583_ebcdic_converter:ebcdic_to_ascii([240,
                                                              241,
                                                              242,
                                                              243,
                                                              244,
                                                              245,
                                                              246,
                                                              247,
                                                              248,
                                                              249]))].

%%====================================================================
%% Test ebcdic_to_ascii/1 - Letters
%%====================================================================

ebcdic_to_ascii_letters_test_() ->
    [% Uppercase
     ?_assertEqual("A", iso_8583_ebcdic_converter:ebcdic_to_ascii([193])),
     ?_assertEqual("J", iso_8583_ebcdic_converter:ebcdic_to_ascii([209])),
     ?_assertEqual("S", iso_8583_ebcdic_converter:ebcdic_to_ascii([226])),
     % Lowercase
     ?_assertEqual("a", iso_8583_ebcdic_converter:ebcdic_to_ascii([129])),
     ?_assertEqual("j", iso_8583_ebcdic_converter:ebcdic_to_ascii([145])),
     ?_assertEqual("s", iso_8583_ebcdic_converter:ebcdic_to_ascii([162]))].

%%====================================================================
%% Test Roundtrip Conversion
%%====================================================================

roundtrip_test_() ->
    [?_test(begin
                Original = "0200",
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic(Original),
                Result = iso_8583_ebcdic_converter:ebcdic_to_ascii(Ebcdic),
                ?assertEqual(Original, Result)
            end),
     ?_test(begin
                Original = "ABCDEFGHIJ",
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic(Original),
                Result = iso_8583_ebcdic_converter:ebcdic_to_ascii(Ebcdic),
                ?assertEqual(Original, Result)
            end),
     ?_test(begin
                Original = "Hello World 123",
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic(Original),
                Result = iso_8583_ebcdic_converter:ebcdic_to_ascii(Ebcdic),
                ?assertEqual(Original, Result)
            end)].

%%====================================================================
%% Test Empty String
%%====================================================================

empty_string_test_() ->
    [?_assertEqual([], iso_8583_ebcdic_converter:ascii_to_ebcdic("")),
     ?_assertEqual("", iso_8583_ebcdic_converter:ebcdic_to_ascii([]))].

%%====================================================================
%% Test ISO 8583 Common Values
%%====================================================================

iso8583_values_test_() ->
    [% MTI values
     ?_test(begin
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic("0100"),
                ?assertEqual([240, 241, 240, 240], Ebcdic)
            end),
     ?_test(begin
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic("0200"),
                ?assertEqual([240, 242, 240, 240], Ebcdic)
            end),
     % Processing code
     ?_test(begin
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic("000000"),
                ?assertEqual([240, 240, 240, 240, 240, 240], Ebcdic)
            end)].
