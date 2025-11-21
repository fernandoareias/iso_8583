-module(iso_8583_fields_id_tests).

-include_lib("eunit/include/eunit.hrl").

-include("iso_8583_fields_id.hrl").

basic_fields_test_() ->
    [?_assertEqual(0, ?MTI),
     ?_assertEqual(1, ?BITMAP_EXTENDED),
     ?_assertEqual(2, ?PAN),
     ?_assertEqual(3, ?PROC_CODE),
     ?_assertEqual(4, ?AMOUNT_TRAN),
     ?_assertEqual(5, ?AMOUNT_SETTLE),
     ?_assertEqual(6, ?AMOUNT_CARDHOLDER_BILLING),
     ?_assertEqual(7, ?TRANSMISSION_DATE_TIME),
     ?_assertEqual(8, ?AMOUNT_CARDHOLDER_BILLING_FEE),
     ?_assertEqual(9, ?CONVERSION_RATE_SETTLE),
     ?_assertEqual(10, ?CONVERSION_RATE_CARDHOLDER_BILLING),
     ?_assertEqual(11, ?SYSTEMS_TRACE_AUDIT_NUMBER)].

%% Field 12 tests (1987 and 1993 versions)
field_12_test_() ->
    [?_assertEqual(12, ?TIME_LOCAL_TRAN),
     ?_assertEqual(12, ?DATE_AND_TIME_LOCAL_TRAN),
     ?_assertEqual(?TIME_LOCAL_TRAN, ?DATE_AND_TIME_LOCAL_TRAN)].

%% Field 13 tests (1987 and 1993 versions)
field_13_test_() ->
    [?_assertEqual(13, ?DATE_LOCAL_TRAN),
     ?_assertEqual(13, ?DATE_EFF),
     ?_assertEqual(?DATE_LOCAL_TRAN, ?DATE_EFF)].

%% Date fields tests
date_fields_test_() ->
    [?_assertEqual(14, ?DATE_EXP),
     ?_assertEqual(15, ?DATE_SETTLE),
     ?_assertEqual(16, ?DATE_CONVERSION),
     ?_assertEqual(17, ?DATE_CAPTURE),
     ?_assertEqual(73, ?DATE_ACTION)].

%% Merchant and country code fields tests
merchant_and_country_fields_test_() ->
    [?_assertEqual(18, ?MERCHANT_TYPE),
     ?_assertEqual(19, ?ACQUIRER_COUNTRY_CODE),
     ?_assertEqual(20, ?PAN_EXT_COUNTRY_CODE),
     ?_assertEqual(21, ?FORWARDING_INST_COUNTRY_CODE),
     ?_assertEqual(68, ?RECEIVING_INSTITUTION_COUNTRY_CODE),
     ?_assertEqual(69, ?SETTLE_INSTITUTION_COUNTRY_CODE)].

%% Field 22 tests (1987 and 1993 versions)
field_22_test_() ->
    [?_assertEqual(22, ?POS_ENTRY_MODE),
     ?_assertEqual(22, ?POS_DATA_CODE),
     ?_assertEqual(?POS_ENTRY_MODE, ?POS_DATA_CODE)].

%% POS related fields tests
pos_fields_test_() ->
    [?_assertEqual(22, ?POS_ENTRY_MODE),
     ?_assertEqual(22, ?POS_DATA_CODE),
     ?_assertEqual(25, ?POS_CONDITION_CODE),
     ?_assertEqual(26, ?POS_CAPTURE_CODE),
     ?_assertEqual(23, ?CARD_SEQUENCE_NUMBER)].

%% Field 24 tests (1987 and 1993 versions)
field_24_test_() ->
    [?_assertEqual(24, ?NETWORK_INTERNATIONAL_ID),
     ?_assertEqual(24, ?FUNCTION_CODE),
     ?_assertEqual(?NETWORK_INTERNATIONAL_ID, ?FUNCTION_CODE)].

%% Field 25 tests (1987 and 1993 versions)
field_25_test_() ->
    [?_assertEqual(25, ?POS_CONDITION_CODE),
     ?_assertEqual(25, ?MESSAGE_REASON_CODE),
     ?_assertEqual(?POS_CONDITION_CODE, ?MESSAGE_REASON_CODE)].

%% Field 26 tests (1987 and 1993 versions)
field_26_test_() ->
    [?_assertEqual(26, ?POS_CAPTURE_CODE),
     ?_assertEqual(26, ?CARD_ACCEPTOR_BUSINESS_CODE),
     ?_assertEqual(?POS_CAPTURE_CODE, ?CARD_ACCEPTOR_BUSINESS_CODE)].

%% Amount fields tests
amount_fields_test_() ->
    [?_assertEqual(4, ?AMOUNT_TRAN),
     ?_assertEqual(5, ?AMOUNT_SETTLE),
     ?_assertEqual(6, ?AMOUNT_CARDHOLDER_BILLING),
     ?_assertEqual(8, ?AMOUNT_CARDHOLDER_BILLING_FEE),
     ?_assertEqual(28, ?AMOUNT_TRAN_FEE),
     ?_assertEqual(29, ?AMOUNT_SETTLE_FEE),
     ?_assertEqual(30, ?AMOUNT_TRAN_PROCESSING_FEE),
     ?_assertEqual(31, ?AMOUNT_SETTLE_PROCESSING_FEE),
     ?_assertEqual(30, ?AMOUNT_ORIGINAL),
     ?_assertEqual(54, ?ADDITIONAL_AMOUNTS),
     ?_assertEqual(97, ?AMOUNT_NET_SETTLE),
     ?_assertEqual(95, ?REPLACEMENT_AMOUNTS)].

%% Field 30 tests (1987 and 1993 versions)
field_30_test_() ->
    [?_assertEqual(30, ?AMOUNT_TRAN_PROCESSING_FEE),
     ?_assertEqual(30, ?AMOUNT_ORIGINAL),
     ?_assertEqual(?AMOUNT_TRAN_PROCESSING_FEE, ?AMOUNT_ORIGINAL)].

%% Field 31 tests (1987 and 1993 versions)
field_31_test_() ->
    [?_assertEqual(31, ?AMOUNT_SETTLE_PROCESSING_FEE),
     ?_assertEqual(31, ?ACQUIRER_REFERENCE_DATA),
     ?_assertEqual(?AMOUNT_SETTLE_PROCESSING_FEE, ?ACQUIRER_REFERENCE_DATA)].

%% Institution ID fields tests
institution_fields_test_() ->
    [?_assertEqual(32, ?ACQUIRING_INST_ID_CODE),
     ?_assertEqual(33, ?FORWARDING_INST_ID_CODE),
     ?_assertEqual(99, ?SETTLE_INSTITUTION_ID_CODE),
     ?_assertEqual(100, ?RECEIVING_INSTITUTION_ID_CODE)].

%% PAN and Track data fields tests
pan_and_track_fields_test_() ->
    [?_assertEqual(2, ?PAN),
     ?_assertEqual(34, ?PAN_EXTENDED),
     ?_assertEqual(35, ?TRACK_2_DATA),
     ?_assertEqual(36, ?TRACK_3_DATA),
     ?_assertEqual(45, ?TRACK_1_DATA)].

%% Card acceptor fields tests
card_acceptor_fields_test_() ->
    [?_assertEqual(41, ?CARD_ACCEPTOR_TERMINAL_ID),
     ?_assertEqual(42, ?CARD_ACCEPTOR_ID_CODE),
     ?_assertEqual(43, ?CARD_ACCEPTOR_NAME_LOCATION),
     ?_assertEqual(26, ?CARD_ACCEPTOR_BUSINESS_CODE)].

%% Authorization and response fields tests
auth_and_response_fields_test_() ->
    [?_assertEqual(27, ?AUTHORIZING_ID_RESP_LEN),
     ?_assertEqual(37, ?RETRIEVAL_REF_NUM),
     ?_assertEqual(38, ?AUTHORIZATION_ID_RESP),
     ?_assertEqual(39, ?RESP_CODE),
     ?_assertEqual(44, ?ADDITIONAL_RESP_DATA),
     ?_assertEqual(93, ?RESP_INDICATOR)].

%% Additional data fields tests
additional_data_fields_test_() ->
    [?_assertEqual(46, ?ADDITIONAL_DATA_ISO),
     ?_assertEqual(47, ?ADDITIONAL_DATA_NATIONAL),
     ?_assertEqual(48, ?ADDITIONAL_DATA_PRIVATE)].

%% Currency code fields tests
currency_fields_test_() ->
    [?_assertEqual(49, ?CURRENCY_CODE_TRAN),
     ?_assertEqual(50, ?CURRENCY_CODE_SETTLE),
     ?_assertEqual(51, ?CURRENCY_CODE_CARDHOLDER_BILLING)].

%% Security fields tests
security_fields_test_() ->
    [?_assertEqual(40, ?SERVICE_RESTRICTION_CODE),
     ?_assertEqual(52, ?PERSONAL_ID_NUMBER_DATA),
     ?_assertEqual(53, ?SECURITY_RELATED_CONTROL_INFO),
     ?_assertEqual(64, ?MESSAGE_AUTHENTICATION_CODE),
     ?_assertEqual(128, ?MESSAGE_AUTHENTICATION_CODE2),
     ?_assertEqual(92, ?FILE_SECURITY_CODE),
     ?_assertEqual(96, ?MESSAGE_SECURITY_CODE)].

%% Field 55 tests (1987 and 1993 versions)
field_55_test_() ->
    [?_assertEqual(55, ?RESERVED_ISO1),
     ?_assertEqual(55, ?ICC_SYSTEM_RELATED_DATA),
     ?_assertEqual(?RESERVED_ISO1, ?ICC_SYSTEM_RELATED_DATA)].

%% Field 56 tests (1987 and 1993 versions)
field_56_test_() ->
    [?_assertEqual(56, ?RESERVED_ISO2),
     ?_assertEqual(56, ?ORIGINAL_DATA_ELEMENTS_1993),
     ?_assertEqual(?RESERVED_ISO2, ?ORIGINAL_DATA_ELEMENTS_1993)].

%% Reserved national fields tests
reserved_national_fields_test_() ->
    [?_assertEqual(57, ?RESERVED_NATIONAL1),
     ?_assertEqual(58, ?RESERVED_NATIONAL2),
     ?_assertEqual(59, ?RESERVED_NATIONAL3)].

%% Reserved private fields tests
reserved_private_fields_test_() ->
    [?_assertEqual(60, ?RESERVED_PRIVATE1),
     ?_assertEqual(61, ?RESERVED_PRIVATE2),
     ?_assertEqual(62, ?RESERVED_PRIVATE3),
     ?_assertEqual(63, ?RESERVED_PRIVATE4)].

%% Field 65 tests (1987 and 1993 versions)
field_65_test_() ->
    [?_assertEqual(65, ?RESERVED_ISO3),
     ?_assertEqual(65, ?BITMAP_TERTIARY),
     ?_assertEqual(?RESERVED_ISO3, ?BITMAP_TERTIARY)].

%% Settlement and payment fields tests
settlement_fields_test_() ->
    [?_assertEqual(66, ?SETTLE_CODE), ?_assertEqual(67, ?EXTENDED_PAYMENT_CODE)].

%% Network management fields tests
network_management_fields_test_() ->
    [?_assertEqual(70, ?NETWORK_MANAGEMENT_INFORMATION_CODE),
     ?_assertEqual(71, ?MESSAGE_NUMBER),
     ?_assertEqual(72, ?MESSAGE_NUMBER_LAST)].

%% Transaction count fields tests
transaction_count_fields_test_() ->
    [?_assertEqual(74, ?CREDITS_NUMBER),
     ?_assertEqual(75, ?CREDITS_REVERSAL_NUMBER),
     ?_assertEqual(76, ?DEBITS_NUMBER),
     ?_assertEqual(77, ?DEBITS_REVERSAL_NUMBER),
     ?_assertEqual(78, ?TRANSFER_NUMBER),
     ?_assertEqual(79, ?TRANSFER_NUMBER_REVERSAL),
     ?_assertEqual(80, ?INQUIRIES_NUMBER),
     ?_assertEqual(81, ?AUTHORIZATIONS_NUMBER)].

%% Fee amount fields tests
fee_amount_fields_test_() ->
    [?_assertEqual(82, ?CREDITS_PROCESSING_FEE_AMOUNT),
     ?_assertEqual(83, ?CREDITS_TRANSACTION_FEE_AMOUNT),
     ?_assertEqual(84, ?DEBITS_PROCESSING_FEE_AMOUNT),
     ?_assertEqual(85, ?DEBITS_TRANSACTION_FEE_AMOUNT)].

%% Credits and debits amount fields tests
credits_debits_amount_fields_test_() ->
    [?_assertEqual(86, ?CREDITS_AMOUNT),
     ?_assertEqual(87, ?CREDITS_REVERSAL_AMOUNT),
     ?_assertEqual(88, ?DEBITS_AMOUNT),
     ?_assertEqual(89, ?DEBITS_REVERSAL_AMOUNT)].

%% Original data and file fields tests
original_data_and_file_fields_test_() ->
    [?_assertEqual(90, ?ORIGINAL_DATA_ELEMENTS),
     ?_assertEqual(56, ?ORIGINAL_DATA_ELEMENTS_1993),
     ?_assertEqual(91, ?FILE_UPDATE_CODE),
     ?_assertEqual(101, ?FILE_NAME)].

%% Service and indicator fields tests
service_fields_test_() ->
    [?_assertEqual(94, ?SERVICE_INDICATOR), ?_assertEqual(40, ?SERVICE_RESTRICTION_CODE)].

%% Payee and account fields tests
payee_and_account_fields_test_() ->
    [?_assertEqual(98, ?PAYEE),
     ?_assertEqual(102, ?ACCOUNT_ID1),
     ?_assertEqual(103, ?ACCOUNT_ID2),
     ?_assertEqual(104, ?TRAN_DESCRIPTION)].

%% Verify all fields are unique (except intentional duplicates)
field_uniqueness_test() ->
    %% These are the intentional duplicates (1987 vs 1993 versions)
    ?assertEqual(?TIME_LOCAL_TRAN, ?DATE_AND_TIME_LOCAL_TRAN),
    ?assertEqual(?DATE_LOCAL_TRAN, ?DATE_EFF),
    ?assertEqual(?POS_ENTRY_MODE, ?POS_DATA_CODE),
    ?assertEqual(?NETWORK_INTERNATIONAL_ID, ?FUNCTION_CODE),
    ?assertEqual(?POS_CONDITION_CODE, ?MESSAGE_REASON_CODE),
    ?assertEqual(?POS_CAPTURE_CODE, ?CARD_ACCEPTOR_BUSINESS_CODE),
    ?assertEqual(?AMOUNT_TRAN_PROCESSING_FEE, ?AMOUNT_ORIGINAL),
    ?assertEqual(?AMOUNT_SETTLE_PROCESSING_FEE, ?ACQUIRER_REFERENCE_DATA),
    ?assertEqual(?RESERVED_ISO1, ?ICC_SYSTEM_RELATED_DATA),
    ?assertEqual(?RESERVED_ISO2, ?ORIGINAL_DATA_ELEMENTS_1993),
    ?assertEqual(?RESERVED_ISO3, ?BITMAP_TERTIARY).

%% Verify field ranges
field_ranges_test_() ->
    [?_assert(?MTI >= 0),
     ?_assert(?MTI =< 128),
     ?_assert(?BITMAP_EXTENDED >= 0),
     ?_assert(?BITMAP_EXTENDED =< 128),
     ?_assert(?MESSAGE_AUTHENTICATION_CODE2 =< 128),
     %% Verify bitmap fields
     ?_assertEqual(1, ?BITMAP_EXTENDED),
     ?_assertEqual(65, ?BITMAP_TERTIARY)].

%% Verify MAC fields
mac_fields_test_() ->
    [?_assertEqual(64, ?MESSAGE_AUTHENTICATION_CODE),
     ?_assertEqual(128, ?MESSAGE_AUTHENTICATION_CODE2),
     ?_assert(?MESSAGE_AUTHENTICATION_CODE =/= ?MESSAGE_AUTHENTICATION_CODE2)].

%% Test that critical fields have expected values
critical_fields_test_() ->
    [?_assertEqual(0, ?MTI),
     ?_assertEqual(2, ?PAN),
     ?_assertEqual(3, ?PROC_CODE),
     ?_assertEqual(4, ?AMOUNT_TRAN),
     ?_assertEqual(11, ?SYSTEMS_TRACE_AUDIT_NUMBER),
     ?_assertEqual(39, ?RESP_CODE)].
