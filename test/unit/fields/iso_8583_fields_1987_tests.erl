-module(iso_8583_fields_1987_tests).

-include_lib("eunit/include/eunit.hrl").

-include("iso_8583_fields_id.hrl").

%%====================================================================
%% Test get_encoding/1 - Field 0 (MTI)
%%====================================================================

get_encoding_mti_test() ->
    ?assertEqual({n, fixed, 4}, iso_8583_fields_1987:get_encoding(?MTI)).

%%====================================================================
%% Test get_encoding/1 - Field 1 (Bitmap Extended)
%%====================================================================

get_encoding_bitmap_extended_test() ->
    ?assertEqual({b, fixed, 64}, iso_8583_fields_1987:get_encoding(?BITMAP_EXTENDED)).

%%====================================================================
%% Test get_encoding/1 - Primary Fields (2-11)
%%====================================================================

get_encoding_primary_fields_test_() ->
    [?_assertEqual({n, llvar, 19}, iso_8583_fields_1987:get_encoding(?PAN)),
     ?_assertEqual({n, fixed, 6}, iso_8583_fields_1987:get_encoding(?PROC_CODE)),
     ?_assertEqual({n, fixed, 12}, iso_8583_fields_1987:get_encoding(?AMOUNT_TRAN)),
     ?_assertEqual({n, fixed, 12}, iso_8583_fields_1987:get_encoding(?AMOUNT_SETTLE)),
     ?_assertEqual({n, fixed, 12},
                   iso_8583_fields_1987:get_encoding(?AMOUNT_CARDHOLDER_BILLING)),
     ?_assertEqual({n, fixed, 10}, iso_8583_fields_1987:get_encoding(?TRANSMISSION_DATE_TIME)),
     ?_assertEqual({n, fixed, 8},
                   iso_8583_fields_1987:get_encoding(?AMOUNT_CARDHOLDER_BILLING_FEE)),
     ?_assertEqual({n, fixed, 8}, iso_8583_fields_1987:get_encoding(?CONVERSION_RATE_SETTLE)),
     ?_assertEqual({n, fixed, 8},
                   iso_8583_fields_1987:get_encoding(?CONVERSION_RATE_CARDHOLDER_BILLING)),
     ?_assertEqual({n, fixed, 6},
                   iso_8583_fields_1987:get_encoding(?SYSTEMS_TRACE_AUDIT_NUMBER))].

%%====================================================================
%% Test get_encoding/1 - ISO 8583:1987 Specific Fields (12-27)
%%====================================================================

get_encoding_1987_specific_fields_test_() ->
    [?_assertEqual({n, fixed, 6}, iso_8583_fields_1987:get_encoding(?TIME_LOCAL_TRAN)),
     ?_assertEqual({n, fixed, 4}, iso_8583_fields_1987:get_encoding(?DATE_LOCAL_TRAN)),
     ?_assertEqual({n, fixed, 4}, iso_8583_fields_1987:get_encoding(?DATE_EXP)),
     ?_assertEqual({n, fixed, 4}, iso_8583_fields_1987:get_encoding(?DATE_SETTLE)),
     ?_assertEqual({n, fixed, 4}, iso_8583_fields_1987:get_encoding(?DATE_CONVERSION)),
     ?_assertEqual({n, fixed, 4}, iso_8583_fields_1987:get_encoding(?DATE_CAPTURE)),
     ?_assertEqual({n, fixed, 4}, iso_8583_fields_1987:get_encoding(?MERCHANT_TYPE)),
     ?_assertEqual({n, fixed, 3}, iso_8583_fields_1987:get_encoding(?ACQUIRER_COUNTRY_CODE)),
     ?_assertEqual({n, fixed, 3}, iso_8583_fields_1987:get_encoding(?PAN_EXT_COUNTRY_CODE)),
     ?_assertEqual({n, fixed, 3},
                   iso_8583_fields_1987:get_encoding(?FORWARDING_INST_COUNTRY_CODE)),
     ?_assertEqual({n, fixed, 3}, iso_8583_fields_1987:get_encoding(?POS_ENTRY_MODE)),
     ?_assertEqual({n, fixed, 3}, iso_8583_fields_1987:get_encoding(?CARD_SEQUENCE_NUMBER)),
     ?_assertEqual({n, fixed, 3},
                   iso_8583_fields_1987:get_encoding(?NETWORK_INTERNATIONAL_ID)),
     ?_assertEqual({n, fixed, 2}, iso_8583_fields_1987:get_encoding(?POS_CONDITION_CODE)),
     ?_assertEqual({n, fixed, 2}, iso_8583_fields_1987:get_encoding(?POS_CAPTURE_CODE)),
     ?_assertEqual({n, fixed, 1},
                   iso_8583_fields_1987:get_encoding(?AUTHORIZING_ID_RESP_LEN))].

%%====================================================================
%% Test get_encoding/1 - Amount Fields (28-31)
%%====================================================================

get_encoding_amount_fields_test_() ->
    [?_assertEqual({x_n, fixed, 8}, iso_8583_fields_1987:get_encoding(?AMOUNT_TRAN_FEE)),
     ?_assertEqual({x_n, fixed, 8}, iso_8583_fields_1987:get_encoding(?AMOUNT_SETTLE_FEE)),
     ?_assertEqual({x_n, fixed, 8},
                   iso_8583_fields_1987:get_encoding(?AMOUNT_TRAN_PROCESSING_FEE)),
     ?_assertEqual({x_n, fixed, 8},
                   iso_8583_fields_1987:get_encoding(?AMOUNT_SETTLE_PROCESSING_FEE))].

%%====================================================================
%% Test get_encoding/1 - Institution and Track Data (32-45)
%%====================================================================

get_encoding_institution_track_data_test_() ->
    [?_assertEqual({n, llvar, 11},
                   iso_8583_fields_1987:get_encoding(?ACQUIRING_INST_ID_CODE)),
     ?_assertEqual({n, llvar, 11},
                   iso_8583_fields_1987:get_encoding(?FORWARDING_INST_ID_CODE)),
     ?_assertEqual({ns, llvar, 28}, iso_8583_fields_1987:get_encoding(?PAN_EXTENDED)),
     ?_assertEqual({z, llvar, 37}, iso_8583_fields_1987:get_encoding(?TRACK_2_DATA)),
     ?_assertEqual({ans, lllvar, 104}, iso_8583_fields_1987:get_encoding(?TRACK_3_DATA)),
     ?_assertEqual({an, fixed, 12}, iso_8583_fields_1987:get_encoding(?RETRIEVAL_REF_NUM)),
     ?_assertEqual({an, fixed, 6}, iso_8583_fields_1987:get_encoding(?AUTHORIZATION_ID_RESP)),
     ?_assertEqual({an, fixed, 2}, iso_8583_fields_1987:get_encoding(?RESP_CODE)),
     ?_assertEqual({an, fixed, 3},
                   iso_8583_fields_1987:get_encoding(?SERVICE_RESTRICTION_CODE)),
     ?_assertEqual({ans, fixed, 8},
                   iso_8583_fields_1987:get_encoding(?CARD_ACCEPTOR_TERMINAL_ID)),
     ?_assertEqual({ans, fixed, 15},
                   iso_8583_fields_1987:get_encoding(?CARD_ACCEPTOR_ID_CODE)),
     ?_assertEqual({ans, fixed, 40},
                   iso_8583_fields_1987:get_encoding(?CARD_ACCEPTOR_NAME_LOCATION)),
     ?_assertEqual({an, llvar, 25}, iso_8583_fields_1987:get_encoding(?ADDITIONAL_RESP_DATA)),
     ?_assertEqual({an, llvar, 76}, iso_8583_fields_1987:get_encoding(?TRACK_1_DATA))].

%%====================================================================
%% Test get_encoding/1 - Additional Data (46-48)
%%====================================================================

get_encoding_additional_data_test_() ->
    [?_assertEqual({ans, lllvar, 999},
                   iso_8583_fields_1987:get_encoding(?ADDITIONAL_DATA_ISO)),
     ?_assertEqual({ans, lllvar, 999},
                   iso_8583_fields_1987:get_encoding(?ADDITIONAL_DATA_NATIONAL)),
     ?_assertEqual({ans, lllvar, 999},
                   iso_8583_fields_1987:get_encoding(?ADDITIONAL_DATA_PRIVATE))].

%%====================================================================
%% Test get_encoding/1 - Currency and Security (49-54)
%%====================================================================

get_encoding_currency_security_test_() ->
    [?_assertEqual({an, fixed, 3}, iso_8583_fields_1987:get_encoding(?CURRENCY_CODE_TRAN)),
     ?_assertEqual({an, fixed, 3}, iso_8583_fields_1987:get_encoding(?CURRENCY_CODE_SETTLE)),
     ?_assertEqual({an, fixed, 3},
                   iso_8583_fields_1987:get_encoding(?CURRENCY_CODE_CARDHOLDER_BILLING)),
     ?_assertEqual({b, fixed, 64},
                   iso_8583_fields_1987:get_encoding(?PERSONAL_ID_NUMBER_DATA)),
     ?_assertEqual({n, fixed, 16},
                   iso_8583_fields_1987:get_encoding(?SECURITY_RELATED_CONTROL_INFO)),
     ?_assertEqual({an, lllvar, 120}, iso_8583_fields_1987:get_encoding(?ADDITIONAL_AMOUNTS))].

%%====================================================================
%% Test get_encoding/1 - Reserved ISO (55-56) - 1987 Definitions
%%====================================================================

get_encoding_reserved_iso_test_() ->
    [?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(?RESERVED_ISO1)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(?RESERVED_ISO2))].

%%====================================================================
%% Test get_encoding/1 - Reserved National and Private (57-64)
%%====================================================================

get_encoding_reserved_national_private_test_() ->
    [?_assertEqual({ans, lllvar, 999},
                   iso_8583_fields_1987:get_encoding(?RESERVED_NATIONAL1)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(?RESERVED_NATIONAL2)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(?RESERVED_NATIONAL3)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(?RESERVED_PRIVATE1)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(?RESERVED_PRIVATE2)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(?RESERVED_PRIVATE3)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(?RESERVED_PRIVATE4)),
     ?_assertEqual({b, fixed, 64},
                   iso_8583_fields_1987:get_encoding(?MESSAGE_AUTHENTICATION_CODE))].

%%====================================================================
%% Test get_encoding/1 - Settlement and Network Management (65-73)
%%====================================================================

get_encoding_settlement_network_test_() ->
    [?_assertEqual({ans, lllvar, 999},
                   iso_8583_fields_1987:get_encoding(65)), % RESERVED_ISO3 in 1987
     ?_assertEqual({n, fixed, 1}, iso_8583_fields_1987:get_encoding(?SETTLE_CODE)),
     ?_assertEqual({n, fixed, 2}, iso_8583_fields_1987:get_encoding(?EXTENDED_PAYMENT_CODE)),
     ?_assertEqual({n, fixed, 3},
                   iso_8583_fields_1987:get_encoding(?RECEIVING_INSTITUTION_COUNTRY_CODE)),
     ?_assertEqual({n, fixed, 3},
                   iso_8583_fields_1987:get_encoding(?SETTLE_INSTITUTION_COUNTRY_CODE)),
     ?_assertEqual({n, fixed, 3},
                   iso_8583_fields_1987:get_encoding(?NETWORK_MANAGEMENT_INFORMATION_CODE)),
     ?_assertEqual({n, fixed, 4}, iso_8583_fields_1987:get_encoding(?MESSAGE_NUMBER)),
     ?_assertEqual({n, fixed, 4}, iso_8583_fields_1987:get_encoding(?MESSAGE_NUMBER_LAST)),
     ?_assertEqual({n, fixed, 6}, iso_8583_fields_1987:get_encoding(?DATE_ACTION))].

%%====================================================================
%% Test get_encoding/1 - Transaction Counts (74-89)
%%====================================================================

get_encoding_transaction_counts_test_() ->
    [?_assertEqual({n, fixed, 10}, iso_8583_fields_1987:get_encoding(?CREDITS_NUMBER)),
     ?_assertEqual({n, fixed, 10},
                   iso_8583_fields_1987:get_encoding(?CREDITS_REVERSAL_NUMBER)),
     ?_assertEqual({n, fixed, 10}, iso_8583_fields_1987:get_encoding(?DEBITS_NUMBER)),
     ?_assertEqual({n, fixed, 10}, iso_8583_fields_1987:get_encoding(?DEBITS_REVERSAL_NUMBER)),
     ?_assertEqual({n, fixed, 10}, iso_8583_fields_1987:get_encoding(?TRANSFER_NUMBER)),
     ?_assertEqual({n, fixed, 10},
                   iso_8583_fields_1987:get_encoding(?TRANSFER_NUMBER_REVERSAL)),
     ?_assertEqual({n, fixed, 10}, iso_8583_fields_1987:get_encoding(?INQUIRIES_NUMBER)),
     ?_assertEqual({n, fixed, 10}, iso_8583_fields_1987:get_encoding(?AUTHORIZATIONS_NUMBER)),
     ?_assertEqual({n, fixed, 12},
                   iso_8583_fields_1987:get_encoding(?CREDITS_PROCESSING_FEE_AMOUNT)),
     ?_assertEqual({n, fixed, 12},
                   iso_8583_fields_1987:get_encoding(?CREDITS_TRANSACTION_FEE_AMOUNT)),
     ?_assertEqual({n, fixed, 12},
                   iso_8583_fields_1987:get_encoding(?DEBITS_PROCESSING_FEE_AMOUNT)),
     ?_assertEqual({n, fixed, 12},
                   iso_8583_fields_1987:get_encoding(?DEBITS_TRANSACTION_FEE_AMOUNT)),
     ?_assertEqual({n, fixed, 16}, iso_8583_fields_1987:get_encoding(?CREDITS_AMOUNT)),
     ?_assertEqual({n, fixed, 16},
                   iso_8583_fields_1987:get_encoding(?CREDITS_REVERSAL_AMOUNT)),
     ?_assertEqual({n, fixed, 16}, iso_8583_fields_1987:get_encoding(?DEBITS_AMOUNT)),
     ?_assertEqual({n, fixed, 16},
                   iso_8583_fields_1987:get_encoding(?DEBITS_REVERSAL_AMOUNT))].

%%====================================================================
%% Test get_encoding/1 - Original Data and File Management (90-104)
%%====================================================================

get_encoding_original_data_file_mgmt_test_() ->
    [?_assertEqual({n, fixed, 42},
                   iso_8583_fields_1987:get_encoding(?ORIGINAL_DATA_ELEMENTS)),
     ?_assertEqual({an, fixed, 1}, iso_8583_fields_1987:get_encoding(?FILE_UPDATE_CODE)),
     ?_assertEqual({an, fixed, 2}, iso_8583_fields_1987:get_encoding(?FILE_SECURITY_CODE)),
     ?_assertEqual({an, fixed, 5}, iso_8583_fields_1987:get_encoding(?RESP_INDICATOR)),
     ?_assertEqual({an, fixed, 7}, iso_8583_fields_1987:get_encoding(?SERVICE_INDICATOR)),
     ?_assertEqual({an, fixed, 42}, iso_8583_fields_1987:get_encoding(?REPLACEMENT_AMOUNTS)),
     ?_assertEqual({b, fixed, 64}, iso_8583_fields_1987:get_encoding(?MESSAGE_SECURITY_CODE)),
     ?_assertEqual({x_n, fixed, 16}, iso_8583_fields_1987:get_encoding(?AMOUNT_NET_SETTLE)),
     ?_assertEqual({ans, fixed, 25}, iso_8583_fields_1987:get_encoding(?PAYEE)),
     ?_assertEqual({n, llvar, 11},
                   iso_8583_fields_1987:get_encoding(?SETTLE_INSTITUTION_ID_CODE)),
     ?_assertEqual({n, llvar, 11},
                   iso_8583_fields_1987:get_encoding(?RECEIVING_INSTITUTION_ID_CODE)),
     ?_assertEqual({ans, llvar, 17}, iso_8583_fields_1987:get_encoding(?FILE_NAME)),
     ?_assertEqual({ans, llvar, 28}, iso_8583_fields_1987:get_encoding(?ACCOUNT_ID1)),
     ?_assertEqual({ans, llvar, 28}, iso_8583_fields_1987:get_encoding(?ACCOUNT_ID2)),
     ?_assertEqual({ans, lllvar, 100}, iso_8583_fields_1987:get_encoding(?TRAN_DESCRIPTION))].

%%====================================================================
%% Test get_encoding/1 - Private Use Fields (105-127)
%%====================================================================

get_encoding_private_use_test_() ->
    [?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(105)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(110)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(115)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(120)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(127))].

%%====================================================================
%% Test get_encoding/1 - Field 128 (MAC 2)
%%====================================================================

get_encoding_mac2_test() ->
    ?assertEqual({b, fixed, 64},
                 iso_8583_fields_1987:get_encoding(?MESSAGE_AUTHENTICATION_CODE2)).

%%====================================================================
%% Test get_encoding/1 - List-based Field IDs
%%====================================================================

get_encoding_list_based_test_() ->
    [?_assertEqual({n, fixed, 4}, iso_8583_fields_1987:get_encoding([?MTI])),
     ?_assertEqual({n, llvar, 19}, iso_8583_fields_1987:get_encoding([?PAN])),
     ?_assertEqual({n, fixed, 6}, iso_8583_fields_1987:get_encoding([?PROC_CODE]))].

%%====================================================================
%% Test get_encoding/1 - Encoding Types
%%====================================================================

encoding_types_test_() ->
    % Test that we have all the expected encoding types
    [% Numeric (n)
     ?_assertEqual(n, element(1, iso_8583_fields_1987:get_encoding(?MTI))),
     ?_assertEqual(n, element(1, iso_8583_fields_1987:get_encoding(?PROC_CODE))),
     % Binary (b)
     ?_assertEqual(b, element(1, iso_8583_fields_1987:get_encoding(?BITMAP_EXTENDED))),
     ?_assertEqual(b,
                   element(1, iso_8583_fields_1987:get_encoding(?MESSAGE_AUTHENTICATION_CODE))),
     % Alphanumeric (an)
     ?_assertEqual(an, element(1, iso_8583_fields_1987:get_encoding(?RETRIEVAL_REF_NUM))),
     ?_assertEqual(an, element(1, iso_8583_fields_1987:get_encoding(?RESP_CODE))),
     % Alphanumeric and special (ans)
     ?_assertEqual(ans,
                   element(1, iso_8583_fields_1987:get_encoding(?CARD_ACCEPTOR_TERMINAL_ID))),
     ?_assertEqual(ans, element(1, iso_8583_fields_1987:get_encoding(?ADDITIONAL_DATA_ISO))),
     % Signed numeric (x_n)
     ?_assertEqual(x_n, element(1, iso_8583_fields_1987:get_encoding(?AMOUNT_TRAN_FEE))),
     % Numeric and special (ns)
     ?_assertEqual(ns, element(1, iso_8583_fields_1987:get_encoding(?PAN_EXTENDED))),
     % Track 2 data (z)
     ?_assertEqual(z, element(1, iso_8583_fields_1987:get_encoding(?TRACK_2_DATA)))].

%%====================================================================
%% Test get_encoding/1 - Length Types
%%====================================================================

length_types_test_() ->
    [% Fixed length
     ?_assertEqual(fixed, element(2, iso_8583_fields_1987:get_encoding(?MTI))),
     ?_assertEqual(fixed, element(2, iso_8583_fields_1987:get_encoding(?PROC_CODE))),
     % Variable length - llvar (2-digit length)
     ?_assertEqual(llvar, element(2, iso_8583_fields_1987:get_encoding(?PAN))),
     ?_assertEqual(llvar, element(2, iso_8583_fields_1987:get_encoding(?TRACK_2_DATA))),
     % Variable length - lllvar (3-digit length)
     ?_assertEqual(lllvar,
                   element(2, iso_8583_fields_1987:get_encoding(?ADDITIONAL_DATA_ISO))),
     ?_assertEqual(lllvar, element(2, iso_8583_fields_1987:get_encoding(?TRAN_DESCRIPTION)))].

%%====================================================================
%% Test get_field_name/1
%%====================================================================

get_field_name_test_() ->
    [?_assertEqual(<<"Message Type Indicator">>, iso_8583_fields_1987:get_field_name(?MTI)),
     ?_assertEqual(<<"Primary Account Number">>, iso_8583_fields_1987:get_field_name(?PAN)),
     ?_assertEqual(<<"Processing Code">>, iso_8583_fields_1987:get_field_name(?PROC_CODE)),
     ?_assertEqual(<<"Transaction Amount">>,
                   iso_8583_fields_1987:get_field_name(?AMOUNT_TRAN)),
     ?_assertEqual(<<"Systems Trace Audit Number">>,
                   iso_8583_fields_1987:get_field_name(?SYSTEMS_TRACE_AUDIT_NUMBER)),
     ?_assertEqual(<<"Time, Local Transaction">>,
                   iso_8583_fields_1987:get_field_name(?TIME_LOCAL_TRAN)),
     ?_assertEqual(<<"Date, Local Transaction">>,
                   iso_8583_fields_1987:get_field_name(?DATE_LOCAL_TRAN)),
     ?_assertEqual(<<"Card Acceptor Terminal ID">>,
                   iso_8583_fields_1987:get_field_name(?CARD_ACCEPTOR_TERMINAL_ID)),
     ?_assertEqual(<<"Response Code">>, iso_8583_fields_1987:get_field_name(?RESP_CODE))].

%%====================================================================
%% Test get_field_name/1 - Private Use Fields
%%====================================================================

get_field_name_private_test_() ->
    [?_assertEqual(<<"Private Use Field 105">>, iso_8583_fields_1987:get_field_name(105)),
     ?_assertEqual(<<"Private Use Field 110">>, iso_8583_fields_1987:get_field_name(110)),
     ?_assertEqual(<<"Private Use Field 127">>, iso_8583_fields_1987:get_field_name(127))].

%%====================================================================
%% Test get_field_name/1 - Generic Fields
%%====================================================================

get_field_name_generic_test_() ->
    [?_assertEqual(<<"Field 14">>, iso_8583_fields_1987:get_field_name(14)),
     ?_assertEqual(<<"Field 50">>, iso_8583_fields_1987:get_field_name(50))].

%%====================================================================
%% Test is_valid_field/1
%%====================================================================

is_valid_field_test_() ->
    [% Valid fields
     ?_assert(iso_8583_fields_1987:is_valid_field(0)),
     ?_assert(iso_8583_fields_1987:is_valid_field(1)),
     ?_assert(iso_8583_fields_1987:is_valid_field(64)),
     ?_assert(iso_8583_fields_1987:is_valid_field(128)),
     % Invalid fields
     ?_assertNot(iso_8583_fields_1987:is_valid_field(-1)),
     ?_assertNot(iso_8583_fields_1987:is_valid_field(129)),
     ?_assertNot(iso_8583_fields_1987:is_valid_field(200))].

%%====================================================================
%% Test get_all_fields/0
%%====================================================================

get_all_fields_test() ->
    AllFields = iso_8583_fields_1987:get_all_fields(),
    ?assertEqual(129, length(AllFields)),
    ?assertEqual(0, hd(AllFields)),
    ?assertEqual(128, lists:last(AllFields)).

get_all_fields_sequential_test() ->
    AllFields = iso_8583_fields_1987:get_all_fields(),
    ?assertEqual(lists:seq(0, 128), AllFields).

%%====================================================================
%% Integration Tests
%%====================================================================

all_fields_have_encoding_test() ->
    AllFields = iso_8583_fields_1987:get_all_fields(),
    lists:foreach(fun(FieldId) ->
                     Encoding = iso_8583_fields_1987:get_encoding(FieldId),
                     ?assert(is_tuple(Encoding)),
                     ?assertEqual(3, tuple_size(Encoding))
                  end,
                  AllFields).

all_fields_are_valid_test() ->
    AllFields = iso_8583_fields_1987:get_all_fields(),
    lists:foreach(fun(FieldId) -> ?assert(iso_8583_fields_1987:is_valid_field(FieldId)) end,
                  AllFields).

all_fields_have_name_test() ->
    AllFields = iso_8583_fields_1987:get_all_fields(),
    lists:foreach(fun(FieldId) ->
                     Name = iso_8583_fields_1987:get_field_name(FieldId),
                     ?assert(is_binary(Name)),
                     ?assert(byte_size(Name) > 0)
                  end,
                  AllFields).

%%====================================================================
%% Edge Cases
%%====================================================================

encoding_consistency_test_() ->
    % Verify that encodings are consistent for fields with same purpose
    [% All amount fields should have similar structure
     ?_assertEqual(12, element(3, iso_8583_fields_1987:get_encoding(?AMOUNT_TRAN))),
     ?_assertEqual(12, element(3, iso_8583_fields_1987:get_encoding(?AMOUNT_SETTLE))),
     ?_assertEqual(12,
                   element(3, iso_8583_fields_1987:get_encoding(?AMOUNT_CARDHOLDER_BILLING))),
     % All country codes should be 3 digits
     ?_assertEqual({n, fixed, 3}, iso_8583_fields_1987:get_encoding(?ACQUIRER_COUNTRY_CODE)),
     ?_assertEqual({n, fixed, 3}, iso_8583_fields_1987:get_encoding(?PAN_EXT_COUNTRY_CODE)),
     ?_assertEqual({n, fixed, 3},
                   iso_8583_fields_1987:get_encoding(?FORWARDING_INST_COUNTRY_CODE)),
     % Both MAC fields should have same encoding
     ?_assertEqual(iso_8583_fields_1987:get_encoding(?MESSAGE_AUTHENTICATION_CODE),
                   iso_8583_fields_1987:get_encoding(?MESSAGE_AUTHENTICATION_CODE2))].

%%====================================================================
%% Verify Specific 1987 Version Characteristics
%%====================================================================

verify_1987_version_specifics_test_() ->
    [% Field 12 in 1987 is TIME_LOCAL_TRAN (6 digits)
     ?_assertEqual({n, fixed, 6}, iso_8583_fields_1987:get_encoding(?TIME_LOCAL_TRAN)),
     % Field 13 in 1987 is DATE_LOCAL_TRAN (4 digits)
     ?_assertEqual({n, fixed, 4}, iso_8583_fields_1987:get_encoding(?DATE_LOCAL_TRAN)),
     % Field 65 in 1987 is RESERVED_ISO3 (lllvar)
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(65)),
     % Fields 105-127 are private use
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_1987:get_encoding(106))].
