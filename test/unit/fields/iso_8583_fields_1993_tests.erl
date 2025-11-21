-module(iso_8583_fields_1993_tests).

-include_lib("eunit/include/eunit.hrl").

-include("iso_8583_fields_id.hrl").

%%====================================================================
%% Test Fields Changed in ISO 8583:1993
%%====================================================================

%% Field 12 - Changed from "Time, Local Transaction" to "Date and Time, Local Transaction"
get_encoding_field_12_test() ->
    % In 1993: 12 digits (YYMMDDHHMMSS)
    ?assertEqual({n, fixed, 12},
                 iso_8583_fields_1993:get_encoding(?DATE_AND_TIME_LOCAL_TRAN)),
    % In 1987: 6 digits (HHMMSS)
    ?assertEqual({n, fixed, 6}, iso_8583_fields_1987:get_encoding(?TIME_LOCAL_TRAN)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_1993:get_encoding(?DATE_AND_TIME_LOCAL_TRAN),
                    iso_8583_fields_1987:get_encoding(?TIME_LOCAL_TRAN)).

%% Field 13 - Changed from "Date, Local Transaction" to "Date, Effective"
get_encoding_field_13_test() ->
    % Both versions use the same encoding (4 digits)
    ?assertEqual({n, fixed, 4}, iso_8583_fields_1993:get_encoding(?DATE_EFF)),
    ?assertEqual({n, fixed, 4}, iso_8583_fields_1987:get_encoding(?DATE_LOCAL_TRAN)),
    % But semantically they're different fields (same macro value)
    ?assertEqual(?DATE_EFF, ?DATE_LOCAL_TRAN).

%% Field 15 - Date, Settlement - Length changed
get_encoding_field_15_test() ->
    % In 1993: 6 digits (YYMMDD)
    ?assertEqual({n, fixed, 6}, iso_8583_fields_1993:get_encoding(?DATE_SETTLE)),
    % In 1987: 4 digits (MMDD)
    ?assertEqual({n, fixed, 4}, iso_8583_fields_1987:get_encoding(?DATE_SETTLE)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_1993:get_encoding(?DATE_SETTLE),
                    iso_8583_fields_1987:get_encoding(?DATE_SETTLE)).

%% Field 22 - Changed from "POS Entry Mode" to "POS Data Code"
get_encoding_field_22_test() ->
    % In 1993: alphanumeric 12 chars
    ?assertEqual({an, fixed, 12}, iso_8583_fields_1993:get_encoding(?POS_DATA_CODE)),
    % In 1987: numeric 3 chars
    ?assertEqual({n, fixed, 3}, iso_8583_fields_1987:get_encoding(?POS_ENTRY_MODE)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_1993:get_encoding(?POS_DATA_CODE),
                    iso_8583_fields_1987:get_encoding(?POS_ENTRY_MODE)).

%% Field 24 - Changed from "Network International ID" to "Function Code"
get_encoding_field_24_test() ->
    % Both versions: numeric 3 chars (same encoding, different meaning)
    ?assertEqual({n, fixed, 3}, iso_8583_fields_1993:get_encoding(?FUNCTION_CODE)),
    ?assertEqual({n, fixed, 3}, iso_8583_fields_1987:get_encoding(?NETWORK_INTERNATIONAL_ID)),
    ?assertEqual(iso_8583_fields_1993:get_encoding(?FUNCTION_CODE),
                 iso_8583_fields_1987:get_encoding(?NETWORK_INTERNATIONAL_ID)).

%% Field 25 - Changed from "POS Condition Code" to "Message Reason Code"
get_encoding_field_25_test() ->
    % In 1993: numeric 4 chars
    ?assertEqual({n, fixed, 4}, iso_8583_fields_1993:get_encoding(?MESSAGE_REASON_CODE)),
    % In 1987: numeric 2 chars
    ?assertEqual({n, fixed, 2}, iso_8583_fields_1987:get_encoding(?POS_CONDITION_CODE)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_1993:get_encoding(?MESSAGE_REASON_CODE),
                    iso_8583_fields_1987:get_encoding(?POS_CONDITION_CODE)).

%% Field 26 - Changed from "POS Capture Code" to "Card Acceptor Business Code"
get_encoding_field_26_test() ->
    % In 1993: numeric 4 chars
    ?assertEqual({n, fixed, 4},
                 iso_8583_fields_1993:get_encoding(?CARD_ACCEPTOR_BUSINESS_CODE)),
    % In 1987: numeric 2 chars
    ?assertEqual({n, fixed, 2}, iso_8583_fields_1987:get_encoding(?POS_CAPTURE_CODE)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_1993:get_encoding(?CARD_ACCEPTOR_BUSINESS_CODE),
                    iso_8583_fields_1987:get_encoding(?POS_CAPTURE_CODE)).

%% Field 30 - Changed from "Amount, Transaction Processing Fee" to "Amount, Original"
get_encoding_field_30_test() ->
    % In 1993: numeric 24 chars
    ?assertEqual({n, fixed, 24}, iso_8583_fields_1993:get_encoding(?AMOUNT_ORIGINAL)),
    % In 1987: x_n 8 chars
    ?assertEqual({x_n, fixed, 8},
                 iso_8583_fields_1987:get_encoding(?AMOUNT_TRAN_PROCESSING_FEE)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_1993:get_encoding(?AMOUNT_ORIGINAL),
                    iso_8583_fields_1987:get_encoding(?AMOUNT_TRAN_PROCESSING_FEE)).

%% Field 31 - Changed from "Amount, Settlement Processing Fee" to "Acquirer Reference Data"
get_encoding_field_31_test() ->
    % In 1993: numeric llvar 48 chars
    ?assertEqual({n, llvar, 48}, iso_8583_fields_1993:get_encoding(?ACQUIRER_REFERENCE_DATA)),
    % In 1987: x_n fixed 8 chars
    ?assertEqual({x_n, fixed, 8},
                 iso_8583_fields_1987:get_encoding(?AMOUNT_SETTLE_PROCESSING_FEE)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_1993:get_encoding(?ACQUIRER_REFERENCE_DATA),
                    iso_8583_fields_1987:get_encoding(?AMOUNT_SETTLE_PROCESSING_FEE)).

%% Field 39 - Response Code - Length changed
get_encoding_field_39_test() ->
    % In 1993: alphanumeric 3 chars
    ?assertEqual({an, fixed, 3}, iso_8583_fields_1993:get_encoding(?RESP_CODE)),
    % In 1987: alphanumeric 2 chars
    ?assertEqual({an, fixed, 2}, iso_8583_fields_1987:get_encoding(?RESP_CODE)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_1993:get_encoding(?RESP_CODE),
                    iso_8583_fields_1987:get_encoding(?RESP_CODE)).

%% Field 43 - Card Acceptor Name/Location - Format changed
get_encoding_field_43_test() ->
    % In 1993: alphanumeric llvar 99 chars
    ?assertEqual({an, llvar, 99},
                 iso_8583_fields_1993:get_encoding(?CARD_ACCEPTOR_NAME_LOCATION)),
    % In 1987: ans fixed 40 chars
    ?assertEqual({ans, fixed, 40},
                 iso_8583_fields_1987:get_encoding(?CARD_ACCEPTOR_NAME_LOCATION)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_1993:get_encoding(?CARD_ACCEPTOR_NAME_LOCATION),
                    iso_8583_fields_1987:get_encoding(?CARD_ACCEPTOR_NAME_LOCATION)).

%% Field 53 - Security Related Control Information - Format changed
get_encoding_field_53_test() ->
    % In 1993: alphanumeric llvar 8 chars
    ?assertEqual({an, llvar, 8},
                 iso_8583_fields_1993:get_encoding(?SECURITY_RELATED_CONTROL_INFO)),
    % In 1987: numeric fixed 16 chars
    ?assertEqual({n, fixed, 16},
                 iso_8583_fields_1987:get_encoding(?SECURITY_RELATED_CONTROL_INFO)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_1993:get_encoding(?SECURITY_RELATED_CONTROL_INFO),
                    iso_8583_fields_1987:get_encoding(?SECURITY_RELATED_CONTROL_INFO)).

%% Field 55 - Changed from "Reserved ISO 1" to "ICC System Related Data" (EMV)
get_encoding_field_55_test() ->
    % Both versions: ans lllvar 999 (same encoding, different purpose in 1993)
    ?assertEqual({ans, lllvar, 999},
                 iso_8583_fields_1993:get_encoding(?ICC_SYSTEM_RELATED_DATA)),
    ?assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(?RESERVED_ISO1)),
    ?assertEqual(iso_8583_fields_1993:get_encoding(?ICC_SYSTEM_RELATED_DATA),
                 iso_8583_fields_1987:get_encoding(?RESERVED_ISO1)).

%% Field 56 - Changed from "Reserved ISO 2" to "Original Data Elements"
get_encoding_field_56_test() ->
    % In 1993: alphanumeric llvar 35 chars
    ?assertEqual({an, llvar, 35},
                 iso_8583_fields_1993:get_encoding(?ORIGINAL_DATA_ELEMENTS_1993)),
    % In 1987: ans lllvar 999 chars
    ?assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(?RESERVED_ISO2)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_1993:get_encoding(?ORIGINAL_DATA_ELEMENTS_1993),
                    iso_8583_fields_1987:get_encoding(?RESERVED_ISO2)).

%% Field 65 - Changed from "Reserved ISO 3" to "Bitmap, Tertiary"
get_encoding_field_65_test() ->
    % In 1993: binary fixed 64
    ?assertEqual({b, fixed, 64}, iso_8583_fields_1993:get_encoding(?BITMAP_TERTIARY)),
    % In 1987: ans lllvar 999
    ?assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(65)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_1993:get_encoding(?BITMAP_TERTIARY),
                    iso_8583_fields_1987:get_encoding(65)).

%% Field 96 - Message Security Code - Format changed
get_encoding_field_96_test() ->
    % In 1993: alphanumeric llvar 18 chars
    ?assertEqual({an, llvar, 18}, iso_8583_fields_1993:get_encoding(?MESSAGE_SECURITY_CODE)),
    % In 1987: binary fixed 64
    ?assertEqual({b, fixed, 64}, iso_8583_fields_1987:get_encoding(?MESSAGE_SECURITY_CODE)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_1993:get_encoding(?MESSAGE_SECURITY_CODE),
                    iso_8583_fields_1987:get_encoding(?MESSAGE_SECURITY_CODE)).

%%====================================================================
%% Test that 1993 Delegates Unchanged Fields to 1987
%%====================================================================

delegation_to_1987_test_() ->
    UnchangedFields =
        [?MTI,
         ?BITMAP_EXTENDED,
         ?PAN,
         ?PROC_CODE,
         ?AMOUNT_TRAN,
         ?SYSTEMS_TRACE_AUDIT_NUMBER,
         ?TRANSMISSION_DATE_TIME,
         ?RETRIEVAL_REF_NUM,
         ?AUTHORIZATION_ID_RESP,
         ?CARD_ACCEPTOR_TERMINAL_ID,
         ?TRACK_2_DATA,
         ?MESSAGE_AUTHENTICATION_CODE,
         ?MESSAGE_AUTHENTICATION_CODE2],
    [?_assertEqual(iso_8583_fields_1987:get_encoding(Field),
                   iso_8583_fields_1993:get_encoding(Field))
     || Field <- UnchangedFields].

%%====================================================================
%% Test List-Based Field IDs
%%====================================================================

get_encoding_list_based_test_() ->
    [?_assertEqual({n, fixed, 12},
                   iso_8583_fields_1993:get_encoding([?DATE_AND_TIME_LOCAL_TRAN])),
     ?_assertEqual({an, fixed, 12}, iso_8583_fields_1993:get_encoding([?POS_DATA_CODE])),
     ?_assertEqual({n, fixed, 24}, iso_8583_fields_1993:get_encoding([?AMOUNT_ORIGINAL]))].

%%====================================================================
%% Test get_field_name/1 - Changed Fields
%%====================================================================

get_field_name_changed_fields_test_() ->
    [?_assertEqual(<<"Date and Time, Local Transaction">>,
                   iso_8583_fields_1993:get_field_name(?DATE_AND_TIME_LOCAL_TRAN)),
     ?_assertEqual(<<"Date, Effective">>, iso_8583_fields_1993:get_field_name(?DATE_EFF)),
     ?_assertEqual(<<"POS Data Code">>, iso_8583_fields_1993:get_field_name(?POS_DATA_CODE)),
     ?_assertEqual(<<"Function Code">>, iso_8583_fields_1993:get_field_name(?FUNCTION_CODE)),
     ?_assertEqual(<<"Message Reason Code">>,
                   iso_8583_fields_1993:get_field_name(?MESSAGE_REASON_CODE)),
     ?_assertEqual(<<"Card Acceptor Business Code">>,
                   iso_8583_fields_1993:get_field_name(?CARD_ACCEPTOR_BUSINESS_CODE)),
     ?_assertEqual(<<"Amount, Original">>,
                   iso_8583_fields_1993:get_field_name(?AMOUNT_ORIGINAL)),
     ?_assertEqual(<<"Acquirer Reference Data">>,
                   iso_8583_fields_1993:get_field_name(?ACQUIRER_REFERENCE_DATA)),
     ?_assertEqual(<<"ICC System Related Data">>,
                   iso_8583_fields_1993:get_field_name(?ICC_SYSTEM_RELATED_DATA)),
     ?_assertEqual(<<"Original Data Elements">>,
                   iso_8583_fields_1993:get_field_name(?ORIGINAL_DATA_ELEMENTS_1993)),
     ?_assertEqual(<<"Bitmap, Tertiary">>,
                   iso_8583_fields_1993:get_field_name(?BITMAP_TERTIARY))].

%%====================================================================
%% Test get_field_name/1 - Delegation to 1987
%%====================================================================

get_field_name_delegation_test_() ->
    [?_assertEqual(iso_8583_fields_1987:get_field_name(?MTI),
                   iso_8583_fields_1993:get_field_name(?MTI)),
     ?_assertEqual(iso_8583_fields_1987:get_field_name(?PAN),
                   iso_8583_fields_1993:get_field_name(?PAN)),
     ?_assertEqual(iso_8583_fields_1987:get_field_name(?PROC_CODE),
                   iso_8583_fields_1993:get_field_name(?PROC_CODE))].

%%====================================================================
%% Test is_valid_field/1
%%====================================================================

is_valid_field_test_() ->
    [% Valid fields
     ?_assert(iso_8583_fields_1993:is_valid_field(0)),
     ?_assert(iso_8583_fields_1993:is_valid_field(1)),
     ?_assert(iso_8583_fields_1993:is_valid_field(64)),
     ?_assert(iso_8583_fields_1993:is_valid_field(128)),
     % Invalid fields
     ?_assertNot(iso_8583_fields_1993:is_valid_field(-1)),
     ?_assertNot(iso_8583_fields_1993:is_valid_field(129)),
     ?_assertNot(iso_8583_fields_1993:is_valid_field(200))].

%%====================================================================
%% Test get_all_fields/0
%%====================================================================

get_all_fields_test() ->
    AllFields = iso_8583_fields_1993:get_all_fields(),
    ?assertEqual(129, length(AllFields)),
    ?assertEqual(0, hd(AllFields)),
    ?assertEqual(128, lists:last(AllFields)).

get_all_fields_sequential_test() ->
    AllFields = iso_8583_fields_1993:get_all_fields(),
    ?assertEqual(lists:seq(0, 128), AllFields).

%%====================================================================
%% Integration Tests
%%====================================================================

all_fields_have_encoding_test() ->
    AllFields = iso_8583_fields_1993:get_all_fields(),
    lists:foreach(fun(FieldId) ->
                     Encoding = iso_8583_fields_1993:get_encoding(FieldId),
                     ?assert(is_tuple(Encoding)),
                     ?assertEqual(3, tuple_size(Encoding))
                  end,
                  AllFields).

all_fields_are_valid_test() ->
    AllFields = iso_8583_fields_1993:get_all_fields(),
    lists:foreach(fun(FieldId) -> ?assert(iso_8583_fields_1993:is_valid_field(FieldId)) end,
                  AllFields).

all_fields_have_name_test() ->
    AllFields = iso_8583_fields_1993:get_all_fields(),
    lists:foreach(fun(FieldId) ->
                     Name = iso_8583_fields_1993:get_field_name(FieldId),
                     ?assert(is_binary(Name)),
                     ?assert(byte_size(Name) > 0)
                  end,
                  AllFields).

%%====================================================================
%% Test 1993-Specific Features
%%====================================================================

%% EMV support (Field 55 - ICC System Related Data)
emv_support_test() ->
    Encoding = iso_8583_fields_1993:get_encoding(?ICC_SYSTEM_RELATED_DATA),
    ?assertEqual({ans, lllvar, 999}, Encoding),
    Name = iso_8583_fields_1993:get_field_name(?ICC_SYSTEM_RELATED_DATA),
    ?assertEqual(<<"ICC System Related Data">>, Name).

%% Tertiary bitmap support (Field 65)
tertiary_bitmap_test() ->
    Encoding = iso_8583_fields_1993:get_encoding(?BITMAP_TERTIARY),
    ?assertEqual({b, fixed, 64}, Encoding),
    Name = iso_8583_fields_1993:get_field_name(?BITMAP_TERTIARY),
    ?assertEqual(<<"Bitmap, Tertiary">>, Name).

%% Extended date/time (Field 12)
extended_datetime_test() ->
    Encoding = iso_8583_fields_1993:get_encoding(?DATE_AND_TIME_LOCAL_TRAN),
    ?assertEqual({n, fixed, 12}, Encoding),
    ?assertEqual(12, element(3, Encoding)), % 12 digits for YYMMDDHHMMSS
    Name = iso_8583_fields_1993:get_field_name(?DATE_AND_TIME_LOCAL_TRAN),
    ?assertEqual(<<"Date and Time, Local Transaction">>, Name).

%%====================================================================
%% Comparison Tests - 1987 vs 1993
%%====================================================================

fields_that_changed_count_test() ->
    % List of field IDs that changed between 1987 and 1993
    ChangedFields = [12, 15, 22, 25, 26, 30, 31, 39, 43, 53, 56, 65, 96],
    Changed =
        lists:filter(fun(FieldId) ->
                        iso_8583_fields_1987:get_encoding(FieldId)
                        =/= iso_8583_fields_1993:get_encoding(FieldId)
                     end,
                     ChangedFields),
    ?assertEqual(length(ChangedFields), length(Changed)).

fields_with_same_encoding_different_meaning_test_() ->
    % Field 13, 24, 55 have same encoding but different semantic meaning
    [?_assertEqual(iso_8583_fields_1987:get_encoding(13),
                   iso_8583_fields_1993:get_encoding(13)),
     ?_assertEqual(iso_8583_fields_1987:get_encoding(24),
                   iso_8583_fields_1993:get_encoding(24)),
     ?_assertEqual(iso_8583_fields_1987:get_encoding(55),
                   iso_8583_fields_1993:get_encoding(55)),
     % But different names
     ?_assertNotEqual(iso_8583_fields_1987:get_field_name(13),
                      iso_8583_fields_1993:get_field_name(13)),
     ?_assertNotEqual(iso_8583_fields_1987:get_field_name(24),
                      iso_8583_fields_1993:get_field_name(24)),
     ?_assertNotEqual(iso_8583_fields_1987:get_field_name(55),
                      iso_8583_fields_1993:get_field_name(55))].

%%====================================================================
%% Encoding Type Changes Tests
%%====================================================================

type_changes_test_() ->
    [% Field 22: n -> an
     ?_assertEqual(n, element(1, iso_8583_fields_1987:get_encoding(22))),
     ?_assertEqual(an, element(1, iso_8583_fields_1993:get_encoding(22))),
     % Field 30: x_n -> n
     ?_assertEqual(x_n, element(1, iso_8583_fields_1987:get_encoding(30))),
     ?_assertEqual(n, element(1, iso_8583_fields_1993:get_encoding(30))),
     % Field 43: ans -> an
     ?_assertEqual(ans, element(1, iso_8583_fields_1987:get_encoding(43))),
     ?_assertEqual(an, element(1, iso_8583_fields_1993:get_encoding(43))),
     % Field 53: n -> an
     ?_assertEqual(n, element(1, iso_8583_fields_1987:get_encoding(53))),
     ?_assertEqual(an, element(1, iso_8583_fields_1993:get_encoding(53))),
     % Field 65: ans -> b
     ?_assertEqual(ans, element(1, iso_8583_fields_1987:get_encoding(65))),
     ?_assertEqual(b, element(1, iso_8583_fields_1993:get_encoding(65))),
     % Field 96: b -> an
     ?_assertEqual(b, element(1, iso_8583_fields_1987:get_encoding(96))),
     ?_assertEqual(an, element(1, iso_8583_fields_1993:get_encoding(96)))].

%%====================================================================
%% Length Type Changes Tests
%%====================================================================

length_type_changes_test_() ->
    [% Field 31: fixed -> llvar
     ?_assertEqual(fixed, element(2, iso_8583_fields_1987:get_encoding(31))),
     ?_assertEqual(llvar, element(2, iso_8583_fields_1993:get_encoding(31))),
     % Field 43: fixed -> llvar
     ?_assertEqual(fixed, element(2, iso_8583_fields_1987:get_encoding(43))),
     ?_assertEqual(llvar, element(2, iso_8583_fields_1993:get_encoding(43))),
     % Field 53: fixed -> llvar
     ?_assertEqual(fixed, element(2, iso_8583_fields_1987:get_encoding(53))),
     ?_assertEqual(llvar, element(2, iso_8583_fields_1993:get_encoding(53))),
     % Field 56: lllvar -> llvar
     ?_assertEqual(lllvar, element(2, iso_8583_fields_1987:get_encoding(56))),
     ?_assertEqual(llvar, element(2, iso_8583_fields_1993:get_encoding(56))),
     % Field 65: lllvar -> fixed
     ?_assertEqual(lllvar, element(2, iso_8583_fields_1987:get_encoding(65))),
     ?_assertEqual(fixed, element(2, iso_8583_fields_1993:get_encoding(65))),
     % Field 96: fixed -> llvar
     ?_assertEqual(fixed, element(2, iso_8583_fields_1987:get_encoding(96))),
     ?_assertEqual(llvar, element(2, iso_8583_fields_1993:get_encoding(96)))].
