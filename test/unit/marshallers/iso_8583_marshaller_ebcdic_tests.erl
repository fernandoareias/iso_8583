-module(iso_8583_marshaller_ebcdic_tests).

-include_lib("eunit/include/eunit.hrl").

-include("iso_8583_types.hrl").

%%====================================================================
%% Note: The iso_8583_marshaller_ebcdic module references erl8583_*
%% modules which don't exist in this codebase. These tests verify
%% the module exports and basic structure. Full integration tests
%% would require fixing the module to use iso_8583_* modules.
%%====================================================================

%%====================================================================
%% Test Module Exports
%%====================================================================

module_exports_test_() ->
    [% Verify all expected functions are exported
     ?_assert(erlang:function_exported(iso_8583_marshaller_ebcdic, marshal, 1)),
     ?_assert(erlang:function_exported(iso_8583_marshaller_ebcdic, unmarshal, 1)),
     ?_assert(erlang:function_exported(iso_8583_marshaller_ebcdic, marshal_field, 3)),
     ?_assert(erlang:function_exported(iso_8583_marshaller_ebcdic, unmarshal_field, 3)),
     ?_assert(erlang:function_exported(iso_8583_marshaller_ebcdic, marshal_mti, 1)),
     ?_assert(erlang:function_exported(iso_8583_marshaller_ebcdic, unmarshal_mti, 1)),
     ?_assert(erlang:function_exported(iso_8583_marshaller_ebcdic, marshal_bitmap, 1)),
     ?_assert(erlang:function_exported(iso_8583_marshaller_ebcdic, unmarshal_bitmap, 1)),
     ?_assert(erlang:function_exported(iso_8583_marshaller_ebcdic, marshal_end, 2)),
     ?_assert(erlang:function_exported(iso_8583_marshaller_ebcdic, unmarshal_end, 2))].

%%====================================================================
%% Test marshal_end/2
%%====================================================================

marshal_end_test() ->
    % marshal_end just returns the marshalled data unchanged
    Message = #iso8583_message{},
    Marshalled = [240, 242, 240, 240], % "0200" in EBCDIC
    Result = iso_8583_marshaller_ebcdic:marshal_end(Message, Marshalled),
    ?assertEqual(Marshalled, Result).

%%====================================================================
%% Test EBCDIC Converter Integration
%%====================================================================

ebcdic_converter_available_test_() ->
    [% Verify the EBCDIC converter module is available
     ?_assert(erlang:function_exported(iso_8583_ebcdic_converter, ascii_to_ebcdic, 1)),
     ?_assert(erlang:function_exported(iso_8583_ebcdic_converter, ebcdic_to_ascii, 1))].

ascii_to_ebcdic_basic_test_() ->
    [% Test basic ASCII to EBCDIC conversion
     ?_test(begin
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic("0"),
                ?assert(is_list(Ebcdic)),
                ?assertEqual(1, length(Ebcdic))
            end),
     ?_test(begin
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic("0200"),
                ?assert(is_list(Ebcdic)),
                ?assertEqual(4, length(Ebcdic))
            end),
     % Test digits 0-9
     ?_test(begin
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic("0123456789"),
                ?assertEqual(10, length(Ebcdic)),
                % EBCDIC digits are F0-F9 (240-249)
                ?assertEqual([240, 241, 242, 243, 244, 245, 246, 247, 248, 249], Ebcdic)
            end)].

ebcdic_to_ascii_basic_test_() ->
    [% Test basic EBCDIC to ASCII conversion
     ?_test(begin
                % EBCDIC F0 = ASCII '0'
                Ascii = iso_8583_ebcdic_converter:ebcdic_to_ascii([240]),
                ?assertEqual("0", Ascii)
            end),
     ?_test(begin
                % EBCDIC F0F2F0F0 = ASCII "0200"
                Ascii = iso_8583_ebcdic_converter:ebcdic_to_ascii([240, 242, 240, 240]),
                ?assertEqual("0200", Ascii)
            end),
     % Test digits F0-F9 -> 0-9
     ?_test(begin
                Ascii =
                    iso_8583_ebcdic_converter:ebcdic_to_ascii([240,
                                                               241,
                                                               242,
                                                               243,
                                                               244,
                                                               245,
                                                               246,
                                                               247,
                                                               248,
                                                               249]),
                ?assertEqual("0123456789", Ascii)
            end)].

ascii_ebcdic_roundtrip_test_() ->
    [% Test roundtrip conversion
     ?_test(begin
                Original = "0200",
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic(Original),
                Ascii = iso_8583_ebcdic_converter:ebcdic_to_ascii(Ebcdic),
                ?assertEqual(Original, Ascii)
            end),
     ?_test(begin
                Original = "1234567890",
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic(Original),
                Ascii = iso_8583_ebcdic_converter:ebcdic_to_ascii(Ebcdic),
                ?assertEqual(Original, Ascii)
            end),
     ?_test(begin
                Original = "ABCDEFGHIJ",
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic(Original),
                Ascii = iso_8583_ebcdic_converter:ebcdic_to_ascii(Ebcdic),
                ?assertEqual(Original, Ascii)
            end)].

%%====================================================================
%% Test EBCDIC Character Codes
%%====================================================================

ebcdic_digit_codes_test_() ->
    [% EBCDIC digit codes are 0xF0-0xF9 (240-249)
     ?_assertEqual([240], iso_8583_ebcdic_converter:ascii_to_ebcdic("0")),
     ?_assertEqual([241], iso_8583_ebcdic_converter:ascii_to_ebcdic("1")),
     ?_assertEqual([242], iso_8583_ebcdic_converter:ascii_to_ebcdic("2")),
     ?_assertEqual([243], iso_8583_ebcdic_converter:ascii_to_ebcdic("3")),
     ?_assertEqual([244], iso_8583_ebcdic_converter:ascii_to_ebcdic("4")),
     ?_assertEqual([245], iso_8583_ebcdic_converter:ascii_to_ebcdic("5")),
     ?_assertEqual([246], iso_8583_ebcdic_converter:ascii_to_ebcdic("6")),
     ?_assertEqual([247], iso_8583_ebcdic_converter:ascii_to_ebcdic("7")),
     ?_assertEqual([248], iso_8583_ebcdic_converter:ascii_to_ebcdic("8")),
     ?_assertEqual([249], iso_8583_ebcdic_converter:ascii_to_ebcdic("9"))].

ebcdic_uppercase_letter_codes_test_() ->
    [% EBCDIC uppercase letters A-I are 0xC1-0xC9 (193-201)
     ?_assertEqual([193], iso_8583_ebcdic_converter:ascii_to_ebcdic("A")),
     ?_assertEqual([194], iso_8583_ebcdic_converter:ascii_to_ebcdic("B")),
     ?_assertEqual([195], iso_8583_ebcdic_converter:ascii_to_ebcdic("C")),
     % EBCDIC uppercase letters J-R are 0xD1-0xD9 (209-217)
     ?_assertEqual([209], iso_8583_ebcdic_converter:ascii_to_ebcdic("J")),
     ?_assertEqual([210], iso_8583_ebcdic_converter:ascii_to_ebcdic("K")),
     % EBCDIC uppercase letters S-Z are 0xE2-0xE9 (226-233)
     ?_assertEqual([226], iso_8583_ebcdic_converter:ascii_to_ebcdic("S")),
     ?_assertEqual([227], iso_8583_ebcdic_converter:ascii_to_ebcdic("T"))].

ebcdic_special_chars_test_() ->
    [% Space is 0x40 (64) in EBCDIC
     ?_assertEqual([64], iso_8583_ebcdic_converter:ascii_to_ebcdic(" ")),
     % Test multiple spaces
     ?_assertEqual([64, 64, 64], iso_8583_ebcdic_converter:ascii_to_ebcdic("   "))].

%%====================================================================
%% Test Empty String Handling
%%====================================================================

empty_string_test_() ->
    [?_assertEqual([], iso_8583_ebcdic_converter:ascii_to_ebcdic("")),
     ?_assertEqual("", iso_8583_ebcdic_converter:ebcdic_to_ascii([]))].

%%====================================================================
%% Test Mixed Content
%%====================================================================

mixed_content_test_() ->
    [% Test alphanumeric string
     ?_test(begin
                Original = "ABC123",
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic(Original),
                Ascii = iso_8583_ebcdic_converter:ebcdic_to_ascii(Ebcdic),
                ?assertEqual(Original, Ascii)
            end),
     % Test with spaces
     ?_test(begin
                Original = "HELLO WORLD",
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic(Original),
                Ascii = iso_8583_ebcdic_converter:ebcdic_to_ascii(Ebcdic),
                ?assertEqual(Original, Ascii)
            end)].

%%====================================================================
%% Test Lowercase Letters
%%====================================================================

lowercase_letter_codes_test_() ->
    [% EBCDIC lowercase a-i are 0x81-0x89 (129-137)
     ?_assertEqual([129], iso_8583_ebcdic_converter:ascii_to_ebcdic("a")),
     ?_assertEqual([130], iso_8583_ebcdic_converter:ascii_to_ebcdic("b")),
     ?_assertEqual([137], iso_8583_ebcdic_converter:ascii_to_ebcdic("i")),
     % EBCDIC lowercase j-r are 0x91-0x99 (145-153)
     ?_assertEqual([145], iso_8583_ebcdic_converter:ascii_to_ebcdic("j")),
     ?_assertEqual([146], iso_8583_ebcdic_converter:ascii_to_ebcdic("k")),
     ?_assertEqual([153], iso_8583_ebcdic_converter:ascii_to_ebcdic("r")),
     % EBCDIC lowercase s-z are 0xA2-0xA9 (162-169)
     ?_assertEqual([162], iso_8583_ebcdic_converter:ascii_to_ebcdic("s")),
     ?_assertEqual([163], iso_8583_ebcdic_converter:ascii_to_ebcdic("t")),
     ?_assertEqual([169], iso_8583_ebcdic_converter:ascii_to_ebcdic("z"))].

lowercase_roundtrip_test_() ->
    [?_test(begin
                Original = "abcdefghijklmnopqrstuvwxyz",
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic(Original),
                Ascii = iso_8583_ebcdic_converter:ebcdic_to_ascii(Ebcdic),
                ?assertEqual(Original, Ascii)
            end)].

%%====================================================================
%% Test Special Characters
%%====================================================================

special_char_punctuation_test_() ->
    [?_assertEqual([75], iso_8583_ebcdic_converter:ascii_to_ebcdic(".")),
     ?_assertEqual([76], iso_8583_ebcdic_converter:ascii_to_ebcdic("<")),
     ?_assertEqual([77], iso_8583_ebcdic_converter:ascii_to_ebcdic("(")),
     ?_assertEqual([78], iso_8583_ebcdic_converter:ascii_to_ebcdic("+")),
     ?_assertEqual([79], iso_8583_ebcdic_converter:ascii_to_ebcdic("|")),
     ?_assertEqual([80], iso_8583_ebcdic_converter:ascii_to_ebcdic("&")),
     ?_assertEqual([90], iso_8583_ebcdic_converter:ascii_to_ebcdic("!")),
     ?_assertEqual([91], iso_8583_ebcdic_converter:ascii_to_ebcdic("$")),
     ?_assertEqual([92], iso_8583_ebcdic_converter:ascii_to_ebcdic("*")),
     ?_assertEqual([93], iso_8583_ebcdic_converter:ascii_to_ebcdic(")")),
     ?_assertEqual([94], iso_8583_ebcdic_converter:ascii_to_ebcdic(";"))].

special_char_misc_test_() ->
    [?_assertEqual([96], iso_8583_ebcdic_converter:ascii_to_ebcdic("-")),
     ?_assertEqual([97], iso_8583_ebcdic_converter:ascii_to_ebcdic("/")),
     ?_assertEqual([107], iso_8583_ebcdic_converter:ascii_to_ebcdic(",")),
     ?_assertEqual([108], iso_8583_ebcdic_converter:ascii_to_ebcdic("%")),
     ?_assertEqual([109], iso_8583_ebcdic_converter:ascii_to_ebcdic("_")),
     ?_assertEqual([110], iso_8583_ebcdic_converter:ascii_to_ebcdic(">")),
     ?_assertEqual([111], iso_8583_ebcdic_converter:ascii_to_ebcdic("?")),
     ?_assertEqual([121], iso_8583_ebcdic_converter:ascii_to_ebcdic("`")),
     ?_assertEqual([122], iso_8583_ebcdic_converter:ascii_to_ebcdic(":")),
     ?_assertEqual([123], iso_8583_ebcdic_converter:ascii_to_ebcdic("#")),
     ?_assertEqual([124], iso_8583_ebcdic_converter:ascii_to_ebcdic("@")),
     ?_assertEqual([125], iso_8583_ebcdic_converter:ascii_to_ebcdic("'")),
     ?_assertEqual([126], iso_8583_ebcdic_converter:ascii_to_ebcdic("=")),
     ?_assertEqual([127], iso_8583_ebcdic_converter:ascii_to_ebcdic("\""))].

special_char_brackets_test_() ->
    [?_assertEqual([161], iso_8583_ebcdic_converter:ascii_to_ebcdic("~")),
     ?_assertEqual([176], iso_8583_ebcdic_converter:ascii_to_ebcdic("^")),
     ?_assertEqual([186], iso_8583_ebcdic_converter:ascii_to_ebcdic("[")),
     ?_assertEqual([187], iso_8583_ebcdic_converter:ascii_to_ebcdic("]")),
     ?_assertEqual([192], iso_8583_ebcdic_converter:ascii_to_ebcdic("{")),
     ?_assertEqual([208], iso_8583_ebcdic_converter:ascii_to_ebcdic("}")),
     ?_assertEqual([224], iso_8583_ebcdic_converter:ascii_to_ebcdic("\\"))].

special_char_roundtrip_test_() ->
    [?_test(begin
                Original = ".<(+|&!$*);-/,%_>?`:#@'=\"",
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic(Original),
                Ascii = iso_8583_ebcdic_converter:ebcdic_to_ascii(Ebcdic),
                ?assertEqual(Original, Ascii)
            end),
     ?_test(begin
                Original = "~^[]{}\\",
                Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic(Original),
                Ascii = iso_8583_ebcdic_converter:ebcdic_to_ascii(Ebcdic),
                ?assertEqual(Original, Ascii)
            end)].
