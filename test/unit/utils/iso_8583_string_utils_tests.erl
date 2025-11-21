-module(iso_8583_string_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Module Exports
%%====================================================================

module_exports_test_() ->
    [?_assert(erlang:function_exported(iso_8583_string_utils, pad_with_trailing_spaces, 2)),
     ?_assert(erlang:function_exported(iso_8583_string_utils, strip_leading_zeroes, 1)),
     ?_assert(erlang:function_exported(iso_8583_string_utils, strip_trailing_spaces, 1)),
     ?_assert(erlang:function_exported(iso_8583_string_utils, strip_leading_spaces, 1))].

%%====================================================================
%% Test pad_with_trailing_spaces/2
%%====================================================================

pad_with_trailing_spaces_test_() ->
    [% Already correct length - no padding
     ?_assertEqual(<<"HELLO">>,
                   iso_8583_string_utils:pad_with_trailing_spaces(<<"HELLO">>, 5)),
     % Pad single space
     ?_assertEqual(<<"HELLO ">>,
                   iso_8583_string_utils:pad_with_trailing_spaces(<<"HELLO">>, 6)),
     % Pad multiple spaces
     ?_assertEqual(<<"HI    ">>, iso_8583_string_utils:pad_with_trailing_spaces(<<"HI">>, 6)),
     % Empty string padding
     ?_assertEqual(<<"   ">>, iso_8583_string_utils:pad_with_trailing_spaces(<<>>, 3)),
     % Single char padding
     ?_assertEqual(<<"A    ">>, iso_8583_string_utils:pad_with_trailing_spaces(<<"A">>, 5))].

%%====================================================================
%% Test strip_leading_zeroes/1
%%====================================================================

strip_leading_zeroes_test_() ->
    [% Single leading zero
     ?_assertEqual("123", iso_8583_string_utils:strip_leading_zeroes("0123")),
     % Multiple leading zeroes
     ?_assertEqual("123", iso_8583_string_utils:strip_leading_zeroes("000123")),
     % All zeroes
     ?_assertEqual("", iso_8583_string_utils:strip_leading_zeroes("0000")),
     % No leading zeroes
     ?_assertEqual("123", iso_8583_string_utils:strip_leading_zeroes("123")),
     % Single zero
     ?_assertEqual("", iso_8583_string_utils:strip_leading_zeroes("0")),
     % Empty string
     ?_assertEqual("", iso_8583_string_utils:strip_leading_zeroes("")),
     % Zero in middle (should preserve)
     ?_assertEqual("1020", iso_8583_string_utils:strip_leading_zeroes("01020"))].

%%====================================================================
%% Test strip_trailing_spaces/1
%%====================================================================

strip_trailing_spaces_test_() ->
    [% Single trailing space
     ?_assertEqual("HELLO", iso_8583_string_utils:strip_trailing_spaces("HELLO ")),
     % Multiple trailing spaces
     ?_assertEqual("HELLO", iso_8583_string_utils:strip_trailing_spaces("HELLO   ")),
     % No trailing spaces
     ?_assertEqual("HELLO", iso_8583_string_utils:strip_trailing_spaces("HELLO")),
     % All spaces
     ?_assertEqual("", iso_8583_string_utils:strip_trailing_spaces("   ")),
     % Empty string
     ?_assertEqual("", iso_8583_string_utils:strip_trailing_spaces("")),
     % Space in middle (should preserve)
     ?_assertEqual("HEL LO", iso_8583_string_utils:strip_trailing_spaces("HEL LO  ")),
     % Leading spaces preserved
     ?_assertEqual("  HELLO", iso_8583_string_utils:strip_trailing_spaces("  HELLO  "))].

%%====================================================================
%% Test strip_leading_spaces/1
%%====================================================================

strip_leading_spaces_test_() ->
    [% Single leading space
     ?_assertEqual("HELLO", iso_8583_string_utils:strip_leading_spaces(" HELLO")),
     % Multiple leading spaces
     ?_assertEqual("HELLO", iso_8583_string_utils:strip_leading_spaces("   HELLO")),
     % No leading spaces
     ?_assertEqual("HELLO", iso_8583_string_utils:strip_leading_spaces("HELLO")),
     % All spaces
     ?_assertEqual("", iso_8583_string_utils:strip_leading_spaces("   ")),
     % Empty string
     ?_assertEqual("", iso_8583_string_utils:strip_leading_spaces("")),
     % Space in middle (should preserve)
     ?_assertEqual("HEL LO", iso_8583_string_utils:strip_leading_spaces("  HEL LO")),
     % Trailing spaces preserved
     ?_assertEqual("HELLO  ", iso_8583_string_utils:strip_leading_spaces("  HELLO  "))].

%%====================================================================
%% Test Combined Operations
%%====================================================================

combined_operations_test_() ->
    [% Strip both leading and trailing
     ?_test(begin
                Str = "  HELLO  ",
                Result =
                    iso_8583_string_utils:strip_leading_spaces(
                        iso_8583_string_utils:strip_trailing_spaces(Str)),
                ?assertEqual("HELLO", Result)
            end),
     % Strip zeroes then spaces
     ?_test(begin
                Str = "00123  ",
                Stripped = iso_8583_string_utils:strip_trailing_spaces(Str),
                Result = iso_8583_string_utils:strip_leading_zeroes(Stripped),
                ?assertEqual("123", Result)
            end)].
