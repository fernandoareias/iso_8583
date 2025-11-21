-module(iso_8583_bitmap_converter_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Module Exports
%%====================================================================

module_exports_test_() ->
    [?_assert(erlang:function_exported(iso_8583_bitmap_converter, list_to_bitmap, 2)),
     ?_assert(erlang:function_exported(iso_8583_bitmap_converter, bitmap_to_list, 2))].

%%====================================================================
%% Test list_to_bitmap/2 - Basic Cases
%%====================================================================

list_to_bitmap_basic_test_() ->
    [% Empty list - all zeros
     ?_assertEqual(<<0, 0, 0, 0, 0, 0, 0, 0>>,
                   iso_8583_bitmap_converter:list_to_bitmap([], 0)),
     % Single field at position 1 (bit 0) with offset 0
     ?_assertEqual(<<128, 0, 0, 0, 0, 0, 0, 0>>,
                   iso_8583_bitmap_converter:list_to_bitmap([1], 0)),
     % Single field at position 64 (last bit) with offset 0
     ?_assertEqual(<<0, 0, 0, 0, 0, 0, 0, 1>>,
                   iso_8583_bitmap_converter:list_to_bitmap([64], 0)),
     % Field 2 (PAN) - second bit
     ?_assertEqual(<<64, 0, 0, 0, 0, 0, 0, 0>>,
                   iso_8583_bitmap_converter:list_to_bitmap([2], 0)),
     % Fields 1 and 2
     ?_assertEqual(<<192, 0, 0, 0, 0, 0, 0, 0>>,
                   iso_8583_bitmap_converter:list_to_bitmap([1, 2], 0))].

%%====================================================================
%% Test list_to_bitmap/2 - With Offset
%%====================================================================

list_to_bitmap_offset_test_() ->
    [% Secondary bitmap - fields 65-128 with offset 64
     ?_assertEqual(<<128, 0, 0, 0, 0, 0, 0, 0>>,
                   iso_8583_bitmap_converter:list_to_bitmap([65], 64)),
     % Field 128 (last in secondary) with offset 64
     ?_assertEqual(<<0, 0, 0, 0, 0, 0, 0, 1>>,
                   iso_8583_bitmap_converter:list_to_bitmap([128], 64)),
     % Multiple fields in secondary bitmap
     ?_assertEqual(<<192, 0, 0, 0, 0, 0, 0, 0>>,
                   iso_8583_bitmap_converter:list_to_bitmap([65, 66], 64))].

%%====================================================================
%% Test list_to_bitmap/2 - Ignores Out of Range
%%====================================================================

list_to_bitmap_out_of_range_test_() ->
    [% Field outside range is ignored (field 65 with offset 0)
     ?_assertEqual(<<128, 0, 0, 0, 0, 0, 0, 0>>,
                   iso_8583_bitmap_converter:list_to_bitmap([1, 65], 0)),
     % Field at offset boundary is ignored (field 0 with offset 0)
     ?_assertEqual(<<0, 0, 0, 0, 0, 0, 0, 0>>,
                   iso_8583_bitmap_converter:list_to_bitmap([0], 0)),
     % Negative field is ignored
     ?_assertEqual(<<0, 0, 0, 0, 0, 0, 0, 0>>,
                   iso_8583_bitmap_converter:list_to_bitmap([-1], 0))].

%%====================================================================
%% Test bitmap_to_list/2 - Basic Cases
%%====================================================================

bitmap_to_list_basic_test_() ->
    [% Empty bitmap
     ?_assertEqual([],
                   iso_8583_bitmap_converter:bitmap_to_list(<<0, 0, 0, 0, 0, 0, 0, 0>>, 0)),
     % Single bit set at position 1
     ?_assertEqual([1],
                   iso_8583_bitmap_converter:bitmap_to_list(<<128, 0, 0, 0, 0, 0, 0, 0>>, 0)),
     % Single bit set at position 64
     ?_assertEqual([64],
                   iso_8583_bitmap_converter:bitmap_to_list(<<0, 0, 0, 0, 0, 0, 0, 1>>, 0)),
     % Multiple bits set
     ?_assertEqual([1, 2],
                   iso_8583_bitmap_converter:bitmap_to_list(<<192, 0, 0, 0, 0, 0, 0, 0>>, 0))].

%%====================================================================
%% Test bitmap_to_list/2 - With Offset
%%====================================================================

bitmap_to_list_offset_test_() ->
    [% Secondary bitmap fields 65-128
     ?_assertEqual([65],
                   iso_8583_bitmap_converter:bitmap_to_list(<<128, 0, 0, 0, 0, 0, 0, 0>>, 64)),
     ?_assertEqual([128],
                   iso_8583_bitmap_converter:bitmap_to_list(<<0, 0, 0, 0, 0, 0, 0, 1>>, 64)),
     ?_assertEqual([65, 66],
                   iso_8583_bitmap_converter:bitmap_to_list(<<192, 0, 0, 0, 0, 0, 0, 0>>, 64))].

%%====================================================================
%% Test Roundtrip Conversion
%%====================================================================

roundtrip_test_() ->
    [% Empty list
     ?_test(begin
                Original = [],
                Bitmap = iso_8583_bitmap_converter:list_to_bitmap(Original, 0),
                Result = iso_8583_bitmap_converter:bitmap_to_list(Bitmap, 0),
                ?assertEqual(Original, Result)
            end),
     % Single field
     ?_test(begin
                Original = [2],
                Bitmap = iso_8583_bitmap_converter:list_to_bitmap(Original, 0),
                Result = iso_8583_bitmap_converter:bitmap_to_list(Bitmap, 0),
                ?assertEqual(Original, Result)
            end),
     % Multiple fields
     ?_test(begin
                Original = [2, 3, 4, 11, 39, 41],
                Bitmap = iso_8583_bitmap_converter:list_to_bitmap(Original, 0),
                Result = iso_8583_bitmap_converter:bitmap_to_list(Bitmap, 0),
                ?assertEqual(lists:sort(Original), lists:sort(Result))
            end),
     % Secondary bitmap roundtrip
     ?_test(begin
                Original = [65, 66, 102, 103],
                Bitmap = iso_8583_bitmap_converter:list_to_bitmap(Original, 64),
                Result = iso_8583_bitmap_converter:bitmap_to_list(Bitmap, 64),
                ?assertEqual(lists:sort(Original), lists:sort(Result))
            end)].

%%====================================================================
%% Test ISO 8583 Common Field Combinations
%%====================================================================

iso8583_common_fields_test_() ->
    [% Typical authorization request fields: 2, 3, 4, 11, 12, 13, 14, 22, 23, 25, 26, 32, 35, 41, 42, 43, 49, 52
     ?_test(begin
                Fields = [2, 3, 4, 11, 12, 13, 14, 22, 23, 25, 26, 32, 35, 41, 42, 43, 49, 52],
                Bitmap = iso_8583_bitmap_converter:list_to_bitmap(Fields, 0),
                ?assertEqual(8, byte_size(Bitmap)),
                Result = iso_8583_bitmap_converter:bitmap_to_list(Bitmap, 0),
                ?assertEqual(lists:sort(Fields), lists:sort(Result))
            end),
     % Fields spanning all 8 bytes
     ?_test(begin
                Fields = [1, 8, 16, 24, 32, 40, 48, 56, 64],
                Bitmap = iso_8583_bitmap_converter:list_to_bitmap(Fields, 0),
                Result = iso_8583_bitmap_converter:bitmap_to_list(Bitmap, 0),
                ?assertEqual(lists:sort(Fields), lists:sort(Result))
            end)].

%%====================================================================
%% Test All 64 Bits
%%====================================================================

all_bits_test() ->
    AllFields = lists:seq(1, 64),
    Bitmap = iso_8583_bitmap_converter:list_to_bitmap(AllFields, 0),
    ?assertEqual(<<255, 255, 255, 255, 255, 255, 255, 255>>, Bitmap),
    Result = iso_8583_bitmap_converter:bitmap_to_list(Bitmap, 0),
    ?assertEqual(AllFields, Result).
