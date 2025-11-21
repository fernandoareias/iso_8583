-module(iso_8583_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("iso_8583_types.hrl").
-include("iso_8583_fields_id.hrl").

%% CT callbacks
-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2,
         end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
%% Test cases
-export([ascii_marshal_field_roundtrip/1, binary_marshal_unmarshal_roundtrip/1,
         xml_marshal_roundtrip/1, hex_converter_chain/1, numeric_bcd_chain/1, ebcdic_roundtrip/1,
         bitmap_field_roundtrip/1, track2_data_roundtrip/1, field_encoding_1987/1,
         field_encoding_1993/1, field_encoding_2003/1, build_authorization_request/1,
         build_financial_request/1, build_reversal_request/1, binary_ascii_field_comparison/1,
         message_field_manipulation/1]).

%%====================================================================
%% CT Callbacks
%%====================================================================

suite() ->
    [{timetrap, {minutes, 5}}].

all() ->
    [{group, marshaller_integration},
     {group, converter_integration},
     {group, field_encoding},
     {group, message_construction},
     {group, cross_format}].

groups() ->
    [{marshaller_integration,
      [parallel],
      [ascii_marshal_field_roundtrip,
       binary_marshal_unmarshal_roundtrip,
       xml_marshal_roundtrip]},
     {converter_integration,
      [parallel],
      [hex_converter_chain,
       numeric_bcd_chain,
       ebcdic_roundtrip,
       bitmap_field_roundtrip,
       track2_data_roundtrip]},
     {field_encoding,
      [parallel],
      [field_encoding_1987, field_encoding_1993, field_encoding_2003]},
     {message_construction,
      [sequence],
      [build_authorization_request, build_financial_request, build_reversal_request]},
     {cross_format, [sequence], [binary_ascii_field_comparison, message_field_manipulation]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Marshaller Integration Tests
%%====================================================================

ascii_marshal_field_roundtrip(_Config) ->
    %% Test ASCII marshaller MTI
    Mti = "0200",
    MarshalledMti = iso_8583_marshaller_ascii:marshal_mti(Mti),
    "0200" = MarshalledMti,

    %% Test field marshal with fixed field
    ProcCode = "123456",
    MarshalledProcCode =
        iso_8583_marshaller_ascii:marshal_field(?PROC_CODE, ProcCode, iso_8583_fields_1987),
    ProcCode = MarshalledProcCode,

    %% Test LLVAR field marshal
    Pan = "4111111111111111",
    MarshalledPan = iso_8583_marshaller_ascii:marshal_field(?PAN, Pan, iso_8583_fields_1987),
    true = is_list(MarshalledPan) orelse is_binary(MarshalledPan),
    ok.

binary_marshal_unmarshal_roundtrip(_Config) ->
    %% Test binary marshaller MTI roundtrip
    Mti = "0200",
    MarshalledMti = iso_8583_marshaller_binary:marshal_mti(Mti),
    {UnmarshalledMti, []} = iso_8583_marshaller_binary:unmarshal_mti(MarshalledMti),
    Mti = UnmarshalledMti,

    %% Test numeric field roundtrip
    ProcCode = "000000",
    MarshalledField =
        iso_8583_marshaller_binary:marshal_field(?PROC_CODE, ProcCode, iso_8583_fields_1987),
    {UnmarshalledValue, [], []} =
        iso_8583_marshaller_binary:unmarshal_field(?PROC_CODE,
                                                   MarshalledField,
                                                   iso_8583_fields_1987),
    ProcCode = UnmarshalledValue,
    ok.

xml_marshal_roundtrip(_Config) ->
    %% Test XML marshaller MTI
    Mti = "0200",
    MarshalledMti = iso_8583_marshaller_xml:marshal_mti(Mti),
    true = lists:prefix("<field", MarshalledMti),
    true = string:str(MarshalledMti, "value=\"0200\"") > 0,

    %% Test XML field marshalling
    FieldValue = "123456",
    MarshalledField =
        iso_8583_marshaller_xml:marshal_field(?PROC_CODE, FieldValue, iso_8583_fields_1987),
    true = string:str(MarshalledField, "id=\"3\"") > 0,
    true = string:str(MarshalledField, "value=\"123456\"") > 0,
    ok.

%%====================================================================
%% Converter Integration Tests
%%====================================================================

hex_converter_chain(_Config) ->
    %% Test UTF8 -> Hex -> UTF8 chain
    Original = <<"HELLO">>,
    Hex = iso_8583_hex_converter:utf8_to_ascii_hex(Original),
    Back = iso_8583_hex_converter:ascii_hex_to_utf8(Hex),
    Original = Back,

    %% Test Binary -> Hex -> Binary chain
    BinOriginal = <<1, 2, 3, 4, 5, 6, 7, 8>>,
    BinHex = iso_8583_hex_converter:binary_to_ascii_hex(BinOriginal),
    BinBack = iso_8583_hex_converter:ascii_hex_to_binary(BinHex),
    BinOriginal = BinBack,

    %% Test Integer -> Hex -> Integer chain
    IntOriginal = 255,
    IntHex = iso_8583_hex_converter:integer_to_ascii_hex(IntOriginal),
    IntBack = iso_8583_hex_converter:ascii_hex_to_integer(IntHex),
    IntOriginal = IntBack,
    ok.

numeric_bcd_chain(_Config) ->
    %% Test Integer -> BCD -> Integer chain
    Original = 123456,
    Bcd = iso_8583_numeric_converter:integer_to_bcd(Original),
    Back = iso_8583_numeric_converter:bcd_to_integer(Bcd),
    Original = Back,

    %% Test UTF8 -> BCD -> UTF8 chain
    Utf8Original = <<"999999">>,
    Utf8Bcd = iso_8583_numeric_converter:numeric_utf8_to_bcd(Utf8Original),
    Utf8Back = iso_8583_numeric_converter:bcd_to_numeric_utf8(Utf8Bcd),
    Utf8Original = Utf8Back,
    ok.

ebcdic_roundtrip(_Config) ->
    %% Test ASCII -> EBCDIC -> ASCII chain
    Original = "HELLO WORLD 123",
    Ebcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic(Original),
    Back = iso_8583_ebcdic_converter:ebcdic_to_ascii(Ebcdic),
    Original = Back,

    %% Test MTI conversion
    MtiOriginal = "0200",
    MtiEbcdic = iso_8583_ebcdic_converter:ascii_to_ebcdic(MtiOriginal),
    [240, 242, 240, 240] = MtiEbcdic,
    MtiBack = iso_8583_ebcdic_converter:ebcdic_to_ascii(MtiEbcdic),
    MtiOriginal = MtiBack,
    ok.

bitmap_field_roundtrip(_Config) ->
    %% Test field list -> bitmap -> field list chain
    Fields = [2, 3, 4, 11, 12, 14, 22, 23, 25, 26, 32, 35, 41, 42, 43, 49],
    Bitmap = iso_8583_bitmap_converter:list_to_bitmap(Fields, 0),
    8 = byte_size(Bitmap),
    BackFields = iso_8583_bitmap_converter:bitmap_to_list(Bitmap, 0),
    Fields = lists:sort(BackFields),

    %% Test secondary bitmap (fields 65-128)
    SecondaryFields = [65, 66, 90, 102, 103],
    SecondaryBitmap = iso_8583_bitmap_converter:list_to_bitmap(SecondaryFields, 64),
    SecondaryBack = iso_8583_bitmap_converter:bitmap_to_list(SecondaryBitmap, 64),
    SecondaryFields = lists:sort(SecondaryBack),
    ok.

track2_data_roundtrip(_Config) ->
    %% Test string -> track2 -> string chain
    Pan = "4111111111111111",
    Track2 = iso_8583_track2_converter:string_to_track2(Pan),
    Back = iso_8583_track2_converter:track2_to_string(Track2, length(Pan)),
    Pan = Back,

    %% Test typical track 2 data
    Track2Data = "1234567890123456789",
    Track2Encoded = iso_8583_track2_converter:string_to_track2(Track2Data),
    Track2Decoded =
        iso_8583_track2_converter:track2_to_string(Track2Encoded, length(Track2Data)),
    Track2Data = Track2Decoded,
    ok.

%%====================================================================
%% Field Encoding Tests
%%====================================================================

field_encoding_1987(_Config) ->
    %% Verify key field encodings for 1987 version
    {n, llvar, 19} = iso_8583_fields_1987:get_encoding(?PAN),
    {n, fixed, 6} = iso_8583_fields_1987:get_encoding(?PROC_CODE),
    {n, fixed, 12} = iso_8583_fields_1987:get_encoding(?AMOUNT_TRAN),
    {n, fixed, 6} = iso_8583_fields_1987:get_encoding(?SYSTEMS_TRACE_AUDIT_NUMBER),
    {an, fixed, 2} = iso_8583_fields_1987:get_encoding(?RESP_CODE),

    %% Verify field name exists
    FieldName = iso_8583_fields_1987:get_field_name(?PAN),
    true = is_binary(FieldName),
    true = byte_size(FieldName) > 0,
    ok.

field_encoding_1993(_Config) ->
    %% Verify 1993 inherits from 1987 with modifications
    {n, llvar, 19} = iso_8583_fields_1993:get_encoding(?PAN),
    {n, fixed, 6} = iso_8583_fields_1993:get_encoding(?PROC_CODE),

    %% Verify field validation
    true = iso_8583_fields_1993:is_valid_field(?PAN),
    true = iso_8583_fields_1993:is_valid_field(?PROC_CODE),
    true = iso_8583_fields_1993:is_valid_field(128),
    false = iso_8583_fields_1993:is_valid_field(129),
    ok.

field_encoding_2003(_Config) ->
    %% Verify 2003 extended fields (129-192)
    true = iso_8583_fields_2003:is_valid_field(129),
    true = iso_8583_fields_2003:is_valid_field(192),
    false = iso_8583_fields_2003:is_valid_field(193),

    %% Verify standard fields still work
    {n, llvar, 19} = iso_8583_fields_2003:get_encoding(?PAN),
    {n, fixed, 6} = iso_8583_fields_2003:get_encoding(?PROC_CODE),
    ok.

%%====================================================================
%% Message Construction Tests
%%====================================================================

build_authorization_request(_Config) ->
    %% Build a typical authorization request message
    Mti = "0100",
    Pan = "4111111111111111",
    ProcCode = "000000",
    Amount = "000000001000",
    Stan = "123456",

    %% Marshal each component
    MarshalledMti = iso_8583_marshaller_ascii:marshal_mti(Mti),
    "0100" = MarshalledMti,

    MarshalledPan = iso_8583_marshaller_ascii:marshal_field(?PAN, Pan, iso_8583_fields_1987),
    true = is_binary(MarshalledPan) orelse is_list(MarshalledPan),

    MarshalledProcCode =
        iso_8583_marshaller_ascii:marshal_field(?PROC_CODE, ProcCode, iso_8583_fields_1987),
    ProcCode = MarshalledProcCode,

    MarshalledAmount =
        iso_8583_marshaller_ascii:marshal_field(?AMOUNT_TRAN, Amount, iso_8583_fields_1987),
    Amount = MarshalledAmount,

    MarshalledStan =
        iso_8583_marshaller_ascii:marshal_field(?SYSTEMS_TRACE_AUDIT_NUMBER,
                                                Stan,
                                                iso_8583_fields_1987),
    Stan = MarshalledStan,

    %% Build bitmap for fields 2, 3, 4, 11
    Bitmap = iso_8583_bitmap_converter:list_to_bitmap([2, 3, 4, 11], 0),
    8 = byte_size(Bitmap),
    ok.

build_financial_request(_Config) ->
    %% Build a typical financial request (0200)
    Mti = "0200",

    %% Binary marshaller test
    BinaryMti = iso_8583_marshaller_binary:marshal_mti(Mti),
    [2, 0] = BinaryMti,

    %% Amount field in binary
    Amount = "1000",
    BinaryAmount =
        iso_8583_marshaller_binary:marshal_field(?AMOUNT_TRAN, Amount, iso_8583_fields_1987),
    true = is_list(BinaryAmount),

    %% Unmarshal and verify
    {UnmarshalledMti, []} = iso_8583_marshaller_binary:unmarshal_mti(BinaryMti),
    "0200" = UnmarshalledMti,
    ok.

build_reversal_request(_Config) ->
    %% Build a reversal request (0400)
    Mti = "0400",
    ProcCode = "040000",

    %% ASCII marshaller
    AsciiMti = iso_8583_marshaller_ascii:marshal_mti(Mti),
    "0400" = AsciiMti,

    AsciiProcCode =
        iso_8583_marshaller_ascii:marshal_field(?PROC_CODE, ProcCode, iso_8583_fields_1987),
    ProcCode = AsciiProcCode,

    %% JSON marshaller
    JsonMti = iso_8583_marshaller_json:marshal_mti(Mti),
    true = string:str(JsonMti, "\"0\"") > 0,
    true = string:str(JsonMti, "\"0400\"") > 0,
    ok.

%%====================================================================
%% Cross-Format Tests
%%====================================================================

binary_ascii_field_comparison(_Config) ->
    %% Test same data through ASCII and Binary marshallers
    Mti = "0200",
    ProcCode = "123456",

    %% Both marshaller MTI marshal functions
    AsciiMti = iso_8583_marshaller_ascii:marshal_mti(Mti),
    BinaryMti = iso_8583_marshaller_binary:marshal_mti(Mti),

    %% Verify MTI values
    "0200" = AsciiMti,
    [2, 0] = BinaryMti,

    %% Both marshaller field marshal functions
    AsciiProcCode =
        iso_8583_marshaller_ascii:marshal_field(?PROC_CODE, ProcCode, iso_8583_fields_1987),
    BinaryProcCode =
        iso_8583_marshaller_binary:marshal_field(?PROC_CODE, ProcCode, iso_8583_fields_1987),

    %% ASCII returns string, Binary returns BCD list
    ProcCode = AsciiProcCode,
    [18, 52, 86] = BinaryProcCode,  % BCD encoding of 123456

    %% Binary unmarshal roundtrip
    {BinaryUnmarshalledMti, []} = iso_8583_marshaller_binary:unmarshal_mti(BinaryMti),
    Mti = BinaryUnmarshalledMti,
    ok.

message_field_manipulation(_Config) ->
    %% Test string utils for field padding/stripping
    Padded = iso_8583_string_utils:pad_with_trailing_spaces(<<"TEST">>, 10),
    10 = byte_size(Padded),
    <<"TEST      ">> = Padded,

    %% Strip operations
    "123" = iso_8583_string_utils:strip_leading_zeroes("000123"),
    "HELLO" = iso_8583_string_utils:strip_trailing_spaces("HELLO   "),
    "WORLD" = iso_8583_string_utils:strip_leading_spaces("   WORLD"),
    ok.
