-module(iso_8583_marshaller_binary_tests).

-include_lib("eunit/include/eunit.hrl").

-include("iso_8583_fields_id.hrl").
-include("iso_8583_types.hrl").

%%====================================================================
%% Test marshal_mti/1
%%====================================================================

marshal_mti_test_() ->
    [?_assertEqual([1, 0], iso_8583_marshaller_binary:marshal_mti("0100")),
     ?_assertEqual([2, 0], iso_8583_marshaller_binary:marshal_mti("0200")),
     ?_assertEqual([4, 0], iso_8583_marshaller_binary:marshal_mti("0400")),
     ?_assertEqual([8, 0], iso_8583_marshaller_binary:marshal_mti("0800"))].

%%====================================================================
%% Test unmarshal_mti/1
%%====================================================================

unmarshal_mti_test_() ->
    [?_assertEqual({"0100", [1, 2, 3]},
                   iso_8583_marshaller_binary:unmarshal_mti([1, 0, 1, 2, 3])),
     ?_assertEqual({"0200", []}, iso_8583_marshaller_binary:unmarshal_mti([2, 0])),
     ?_assertEqual({"0800", [255]}, iso_8583_marshaller_binary:unmarshal_mti([8, 0, 255]))].

%%====================================================================
%% Test marshal_field/3 - Numeric Fixed
%%====================================================================

marshal_field_n_fixed_test_() ->
    [% Field 3 - Processing Code (n, fixed, 6)
     ?_assertEqual([0, 0, 0],
                   iso_8583_marshaller_binary:marshal_field(?PROC_CODE, "0", iso_8583_fields_1987)),
     ?_assertEqual([18, 52, 86],
                   iso_8583_marshaller_binary:marshal_field(?PROC_CODE,
                                                            "123456",
                                                            iso_8583_fields_1987)),
     % Field 4 - Amount (n, fixed, 12)
     ?_assertEqual([0, 0, 0, 0, 16, 0],
                   iso_8583_marshaller_binary:marshal_field(?AMOUNT_TRAN,
                                                            "1000",
                                                            iso_8583_fields_1987)),
     ?_assertEqual([0, 0, 0, 0, 0, 1],
                   iso_8583_marshaller_binary:marshal_field(?AMOUNT_TRAN,
                                                            "1",
                                                            iso_8583_fields_1987)),
     % Field 11 - STAN (n, fixed, 6)
     ?_assertEqual([0, 0, 1],
                   iso_8583_marshaller_binary:marshal_field(?SYSTEMS_TRACE_AUDIT_NUMBER,
                                                            "1",
                                                            iso_8583_fields_1987)),
     ?_assertEqual([18, 52, 86],
                   iso_8583_marshaller_binary:marshal_field(?SYSTEMS_TRACE_AUDIT_NUMBER,
                                                            "123456",
                                                            iso_8583_fields_1987))].

%%====================================================================
%% Test marshal_field/3 - Numeric LLVAR
%%====================================================================

marshal_field_n_llvar_test_() ->
    [% Field 2 - PAN (n, llvar, 19) - length 16 in BCD = 22 (0x16)
     ?_assertEqual([22, 65, 17, 17, 17, 17, 17, 17, 17],
                   iso_8583_marshaller_binary:marshal_field(?PAN,
                                                            "4111111111111111",
                                                            iso_8583_fields_1987)),
     ?_assertEqual([25, 1, 35, 69, 103, 137, 1, 35, 69, 103, 137],
                   iso_8583_marshaller_binary:marshal_field(?PAN,
                                                            "1234567890123456789",
                                                            iso_8583_fields_1987))].

%%====================================================================
%% Test marshal_field/3 - Alphanumeric LLVAR
%%====================================================================

marshal_field_an_llvar_test_() ->
    [% Field 44 - Additional Response Data (an, llvar, 25)
     ?_assertEqual([5, $H, $E, $L, $L, $O],
                   iso_8583_marshaller_binary:marshal_field(?ADDITIONAL_RESP_DATA,
                                                            "HELLO",
                                                            iso_8583_fields_1987)),
     ?_assertEqual([16] ++ "ABCDEFGHIJ",
                   iso_8583_marshaller_binary:marshal_field(?ADDITIONAL_RESP_DATA,
                                                            "ABCDEFGHIJ",
                                                            iso_8583_fields_1987))].

%%====================================================================
%% Test marshal_field/3 - ANS LLVAR
%%====================================================================

marshal_field_ans_llvar_test_() ->
    [% Field 102 - Account ID 1 (ans, llvar, 28)
     ?_assertEqual([16] ++ "1234567890",
                   iso_8583_marshaller_binary:marshal_field(?ACCOUNT_ID1,
                                                            "1234567890",
                                                            iso_8583_fields_1987)),
     ?_assertEqual([5] ++ "ACCT1",
                   iso_8583_marshaller_binary:marshal_field(?ACCOUNT_ID1,
                                                            "ACCT1",
                                                            iso_8583_fields_1987))].

%%====================================================================
%% Test marshal_field/3 - ANS LLLVAR
%%====================================================================

marshal_field_ans_lllvar_test_() ->
    [% Field 46 - Additional Data ISO (ans, lllvar, 999)
     ?_assertEqual([0, 5] ++ "HELLO",
                   iso_8583_marshaller_binary:marshal_field(?ADDITIONAL_DATA_ISO,
                                                            "HELLO",
                                                            iso_8583_fields_1987)),
     ?_assertEqual([0, 16] ++ "ABCDEFGHIJ",
                   iso_8583_marshaller_binary:marshal_field(?ADDITIONAL_DATA_ISO,
                                                            "ABCDEFGHIJ",
                                                            iso_8583_fields_1987))].

%%====================================================================
%% Test marshal_field/3 - X+N (Signed Amount)
%%====================================================================

marshal_field_x_n_fixed_test_() ->
    [% Field 28 - Amount Transaction Fee (x_n, fixed, 8)
     ?_assertEqual([$C, 0, 0, 16, 0],
                   iso_8583_marshaller_binary:marshal_field(?AMOUNT_TRAN_FEE,
                                                            "C1000",
                                                            iso_8583_fields_1987)),
     ?_assertEqual([$D, 0, 0, 5, 0],
                   iso_8583_marshaller_binary:marshal_field(?AMOUNT_TRAN_FEE,
                                                            "D500",
                                                            iso_8583_fields_1987))].

%%====================================================================
%% Test marshal_field/3 - Binary Fixed
%%====================================================================

marshal_field_b_fixed_test_() ->
    [% Field 52 - PIN Data (b, fixed, 64)
     ?_assertEqual([1, 2, 3, 4, 5, 6, 7, 8],
                   iso_8583_marshaller_binary:marshal_field(?PERSONAL_ID_NUMBER_DATA,
                                                            <<1, 2, 3, 4, 5, 6, 7, 8>>,
                                                            iso_8583_fields_1987)),
     ?_assertEqual([255, 255, 255, 255, 255, 255, 255, 255],
                   iso_8583_marshaller_binary:marshal_field(?PERSONAL_ID_NUMBER_DATA,
                                                            <<255,
                                                              255,
                                                              255,
                                                              255,
                                                              255,
                                                              255,
                                                              255,
                                                              255>>,
                                                            iso_8583_fields_1987))].

%%====================================================================
%% Test unmarshal_field/3 - Numeric Fixed
%%====================================================================

unmarshal_field_n_fixed_test_() ->
    [% Field 3 - Processing Code (n, fixed, 6)
     ?_assertEqual({"000000", [1, 2, 3], []},
                   iso_8583_marshaller_binary:unmarshal_field(?PROC_CODE,
                                                              [0, 0, 0, 1, 2, 3],
                                                              iso_8583_fields_1987)),
     ?_assertEqual({"123456", [], []},
                   iso_8583_marshaller_binary:unmarshal_field(?PROC_CODE,
                                                              [18, 52, 86],
                                                              iso_8583_fields_1987)),
     % Field 4 - Amount (n, fixed, 12)
     ?_assertEqual({"000000001000", [], []},
                   iso_8583_marshaller_binary:unmarshal_field(?AMOUNT_TRAN,
                                                              [0, 0, 0, 0, 16, 0],
                                                              iso_8583_fields_1987))].

%%====================================================================
%% Test unmarshal_field/3 - Numeric LLVAR
%%====================================================================

unmarshal_field_n_llvar_test_() ->
    % Field 2 - PAN (n, llvar, 19) - length byte is BCD
    [?_assertEqual({"4111111111111111", [1, 2, 3], []},
                   iso_8583_marshaller_binary:unmarshal_field(?PAN,
                                                              [22,
                                                               65,
                                                               17,
                                                               17,
                                                               17,
                                                               17,
                                                               17,
                                                               17,
                                                               17,
                                                               1,
                                                               2,
                                                               3],
                                                              iso_8583_fields_1987))].

%%====================================================================
%% Test unmarshal_field/3 - Alphanumeric LLVAR
%%====================================================================

unmarshal_field_an_llvar_test_() ->
    % Field 44 - Additional Response Data (an, llvar, 25)
    [?_assertEqual({"HELLO", [1, 2, 3], []},
                   iso_8583_marshaller_binary:unmarshal_field(?ADDITIONAL_RESP_DATA,
                                                              [5, $H, $E, $L, $L, $O, 1, 2, 3],
                                                              iso_8583_fields_1987))].

%%====================================================================
%% Test unmarshal_field/3 - ANS LLLVAR
%%====================================================================

unmarshal_field_ans_lllvar_test_() ->
    % Field 46 - Additional Data ISO (ans, lllvar, 999)
    [?_assertEqual({"HELLO", [], []},
                   iso_8583_marshaller_binary:unmarshal_field(?ADDITIONAL_DATA_ISO,
                                                              [0, 5, $H, $E, $L, $L, $O],
                                                              iso_8583_fields_1987))].

%%====================================================================
%% Test unmarshal_field/3 - Binary Fixed
%%====================================================================

unmarshal_field_b_fixed_test_() ->
    % Field 52 - PIN Data (b, fixed, 64)
    [?_assertEqual({<<1, 2, 3, 4, 5, 6, 7, 8>>, [9, 10], []},
                   iso_8583_marshaller_binary:unmarshal_field(?PERSONAL_ID_NUMBER_DATA,
                                                              [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
                                                              iso_8583_fields_1987))].

%%====================================================================
%% Test unmarshal_field/3 - Field 1 (Secondary Bitmap)
%%====================================================================

unmarshal_field_1_bitmap_test() ->
    % Field 1 returns additional field IDs from secondary bitmap
    {Value, Rest, FieldIds} =
        iso_8583_marshaller_binary:unmarshal_field(1,
                                                   [128, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3],
                                                   iso_8583_fields_1987),
    ?assert(is_binary(Value)),
    ?assertEqual([1, 2, 3], Rest),
    ?assert(is_list(FieldIds)),
    ?assert(lists:member(65, FieldIds)). % First bit set = field 65

%%====================================================================
%% Test unmarshal_bitmap/1
%%====================================================================

unmarshal_bitmap_test_() ->
    [% Bitmap with fields 2, 3, 4 set (bits 2, 3, 4 = 0x70)
     ?_test(begin
                {FieldIds, Rest} =
                    iso_8583_marshaller_binary:unmarshal_bitmap([112,
                                                                 0,
                                                                 0,
                                                                 0,
                                                                 0,
                                                                 0,
                                                                 0,
                                                                 0,
                                                                 1,
                                                                 2,
                                                                 3]),
                ?assert(lists:member(2, FieldIds)),
                ?assert(lists:member(3, FieldIds)),
                ?assert(lists:member(4, FieldIds)),
                ?assertEqual([1, 2, 3], Rest)
            end),
     % Empty bitmap (no fields)
     ?_test(begin
                {FieldIds, Rest} =
                    iso_8583_marshaller_binary:unmarshal_bitmap([0, 0, 0, 0, 0, 0, 0, 0, 99]),
                ?assertEqual([], FieldIds),
                ?assertEqual([99], Rest)
            end)].

%%====================================================================
%% Test marshal_end/2
%%====================================================================

marshal_end_test() ->
    Message = #iso8583_message{},
    Marshalled = [0, 2, 0, 0, 112, 0, 0, 0, 0, 0, 0, 0],
    Result = iso_8583_marshaller_binary:marshal_end(Message, Marshalled),
    ?assertEqual(Marshalled, Result).

%%====================================================================
%% Test unmarshal_end/2
%%====================================================================

unmarshal_end_test() ->
    Message = #iso8583_message{},
    Result = iso_8583_marshaller_binary:unmarshal_end(Message, []),
    ?assertEqual(Message, Result).

%%====================================================================
%% Test marshal_bitmap/1
%%====================================================================

marshal_bitmap_basic_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set(0, <<"0200">>, Msg0),
    Msg2 = iso_8583:set(2, <<"4111111111111111">>, Msg1),
    Msg3 = iso_8583:set(3, <<"000000">>, Msg2),
    {Bitmap, _UpdatedMsg} = iso_8583_marshaller_binary:marshal_bitmap(Msg3),
    ?assert(is_list(Bitmap)).

marshal_bitmap_with_secondary_test() ->
    % When field > 64, secondary bitmap is needed
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set(0, <<"0200">>, Msg0),
    Msg2 = iso_8583:set(2, <<"4111111111111111">>, Msg1),
    Msg3 = iso_8583:set(102, <<"ACCOUNT">>, Msg2),  % Field > 64
    {Bitmap, UpdatedMsg} = iso_8583_marshaller_binary:marshal_bitmap(Msg3),
    ?assert(is_list(Bitmap)),
    ?assert(iso_8583:has_field(1, UpdatedMsg)).  % Secondary bitmap field added

%%====================================================================
%% Test marshal_unmarshal_roundtrip MTI
%%====================================================================

marshal_unmarshal_roundtrip_mti_test() ->
    Original = "0200",
    Marshalled = iso_8583_marshaller_binary:marshal_mti(Original),
    {Unmarshalled, []} = iso_8583_marshaller_binary:unmarshal_mti(Marshalled),
    ?assertEqual(Original, Unmarshalled).

%%====================================================================
%% Test Empty LLVAR Fields
%%====================================================================

empty_llvar_field_test_() ->
    [% Empty an llvar
     ?_assertEqual([0],
                   iso_8583_marshaller_binary:marshal_field(?ADDITIONAL_RESP_DATA,
                                                            "",
                                                            iso_8583_fields_1987)),
     % Empty ans lllvar
     ?_assertEqual([0, 0],
                   iso_8583_marshaller_binary:marshal_field(?ADDITIONAL_DATA_ISO,
                                                            "",
                                                            iso_8583_fields_1987))].

%%====================================================================
%% Test X+N Unmarshal
%%====================================================================

unmarshal_field_x_n_fixed_test_() ->
    [% Field 28 - Amount Transaction Fee (x_n, fixed, 8)
     ?_assertEqual({"C00001000", [], []},
                   iso_8583_marshaller_binary:unmarshal_field(?AMOUNT_TRAN_FEE,
                                                              [$C, 0, 0, 16, 0],
                                                              iso_8583_fields_1987)),
     ?_assertEqual({"D00000500", [], []},
                   iso_8583_marshaller_binary:unmarshal_field(?AMOUNT_TRAN_FEE,
                                                              [$D, 0, 0, 5, 0],
                                                              iso_8583_fields_1987))].

%%====================================================================
%% Test Binary with different field values
%%====================================================================

binary_field_values_test_() ->
    [% All zeros
     ?_assertEqual([0, 0, 0, 0, 0, 0, 0, 0],
                   iso_8583_marshaller_binary:marshal_field(?PERSONAL_ID_NUMBER_DATA,
                                                            <<0, 0, 0, 0, 0, 0, 0, 0>>,
                                                            iso_8583_fields_1987)),
     % Mixed values
     ?_assertEqual([1, 35, 69, 103, 137, 171, 205, 239],
                   iso_8583_marshaller_binary:marshal_field(?PERSONAL_ID_NUMBER_DATA,
                                                            <<1, 35, 69, 103, 137, 171, 205, 239>>,
                                                            iso_8583_fields_1987))].
