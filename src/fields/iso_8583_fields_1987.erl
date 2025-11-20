-module(iso_8583_fields_1987).

%%
%% ISO 8583:1987 Field Encoding Definitions
%%
%% This module defines field encodings for the 1987 version of ISO 8583.
%%

%%
%% Include files
%%
-include("iso_8583_types.hrl").
-include("iso_8583_fields_id.hrl").

%%
%% Exported Functions
%%
-export([
    get_encoding/1,
    get_field_name/1,
    is_valid_field/1,
    get_all_fields/0
]).

%%
%% API Functions
%%

%% @doc Returns how a field is encoded as a triple consisting of the content type,
%%      format, and maximum length.
%%
%% @spec get_encoding(FieldId::integer()|list(integer())) -> field_encoding()
-spec(get_encoding(integer() | list(integer())) -> field_encoding()).

%% Field 0 - Message Type Indicator
get_encoding(?MTI) ->
    {n, fixed, 4};

%% Field 1 - Bitmap (Extended)
get_encoding(?BITMAP_EXTENDED) ->
    {b, fixed, 64};

%% Primary Fields (2-11)
get_encoding(?PAN) ->
    {n, llvar, 19};
get_encoding(?PROC_CODE) ->
    {n, fixed, 6};
get_encoding(?AMOUNT_TRAN) ->
    {n, fixed, 12};
get_encoding(?AMOUNT_SETTLE) ->
    {n, fixed, 12};
get_encoding(?AMOUNT_CARDHOLDER_BILLING) ->
    {n, fixed, 12};
get_encoding(?TRANSMISSION_DATE_TIME) ->
    {n, fixed, 10};
get_encoding(?AMOUNT_CARDHOLDER_BILLING_FEE) ->
    {n, fixed, 8};
get_encoding(?CONVERSION_RATE_SETTLE) ->
    {n, fixed, 8};
get_encoding(?CONVERSION_RATE_CARDHOLDER_BILLING) ->
    {n, fixed, 8};
get_encoding(?SYSTEMS_TRACE_AUDIT_NUMBER) ->
    {n, fixed, 6};

%% ISO 8583:1987 specific fields (12-27)
get_encoding(?TIME_LOCAL_TRAN) ->  % Field 12 in 1987
    {n, fixed, 6};
get_encoding(?DATE_LOCAL_TRAN) ->  % Field 13 in 1987
    {n, fixed, 4};
get_encoding(?DATE_EXP) ->
    {n, fixed, 4};
get_encoding(?DATE_SETTLE) ->
    {n, fixed, 4};
get_encoding(?DATE_CONVERSION) ->
    {n, fixed, 4};
get_encoding(?DATE_CAPTURE) ->
    {n, fixed, 4};
get_encoding(?MERCHANT_TYPE) ->
    {n, fixed, 4};
get_encoding(?ACQUIRER_COUNTRY_CODE) ->
    {n, fixed, 3};
get_encoding(?PAN_EXT_COUNTRY_CODE) ->
    {n, fixed, 3};
get_encoding(?FORWARDING_INST_COUNTRY_CODE) ->
    {n, fixed, 3};
get_encoding(?POS_ENTRY_MODE) ->  % Field 22 in 1987
    {n, fixed, 3};
get_encoding(?CARD_SEQUENCE_NUMBER) ->
    {n, fixed, 3};
get_encoding(?NETWORK_INTERNATIONAL_ID) ->  % Field 24 in 1987
    {n, fixed, 3};
get_encoding(?POS_CONDITION_CODE) ->  % Field 25 in 1987
    {n, fixed, 2};
get_encoding(?POS_CAPTURE_CODE) ->  % Field 26 in 1987
    {n, fixed, 2};
get_encoding(?AUTHORIZING_ID_RESP_LEN) ->
    {n, fixed, 1};

%% Amount fields (28-31)
get_encoding(?AMOUNT_TRAN_FEE) ->
    {x_n, fixed, 8};
get_encoding(?AMOUNT_SETTLE_FEE) ->
    {x_n, fixed, 8};
get_encoding(?AMOUNT_TRAN_PROCESSING_FEE) ->  % Field 30 in 1987
    {x_n, fixed, 8};
get_encoding(?AMOUNT_SETTLE_PROCESSING_FEE) ->  % Field 31 in 1987
    {x_n, fixed, 8};

%% Institution and track data (32-45)
get_encoding(?ACQUIRING_INST_ID_CODE) ->
    {n, llvar, 11};
get_encoding(?FORWARDING_INST_ID_CODE) ->
    {n, llvar, 11};
get_encoding(?PAN_EXTENDED) ->
    {ns, llvar, 28};
get_encoding(?TRACK_2_DATA) ->
    {z, llvar, 37};
get_encoding(?TRACK_3_DATA) ->
    {ans, lllvar, 104};
get_encoding(?RETRIEVAL_REF_NUM) ->
    {an, fixed, 12};
get_encoding(?AUTHORIZATION_ID_RESP) ->
    {an, fixed, 6};
get_encoding(?RESP_CODE) ->
    {an, fixed, 2};
get_encoding(?SERVICE_RESTRICTION_CODE) ->
    {an, fixed, 3};
get_encoding(?CARD_ACCEPTOR_TERMINAL_ID) ->
    {ans, fixed, 8};
get_encoding(?CARD_ACCEPTOR_ID_CODE) ->
    {ans, fixed, 15};
get_encoding(?CARD_ACCEPTOR_NAME_LOCATION) ->
    {ans, fixed, 40};
get_encoding(?ADDITIONAL_RESP_DATA) ->
    {an, llvar, 25};
get_encoding(?TRACK_1_DATA) ->
    {an, llvar, 76};

%% Additional data (46-48)
get_encoding(?ADDITIONAL_DATA_ISO) ->
    {ans, lllvar, 999};
get_encoding(?ADDITIONAL_DATA_NATIONAL) ->
    {ans, lllvar, 999};
get_encoding(?ADDITIONAL_DATA_PRIVATE) ->
    {ans, lllvar, 999};

%% Currency and security (49-54)
get_encoding(?CURRENCY_CODE_TRAN) ->
    {an, fixed, 3};
get_encoding(?CURRENCY_CODE_SETTLE) ->
    {an, fixed, 3};
get_encoding(?CURRENCY_CODE_CARDHOLDER_BILLING) ->
    {an, fixed, 3};
get_encoding(?PERSONAL_ID_NUMBER_DATA) ->
    {b, fixed, 64};
get_encoding(?SECURITY_RELATED_CONTROL_INFO) ->
    {n, fixed, 16};
get_encoding(?ADDITIONAL_AMOUNTS) ->
    {an, lllvar, 120};

%% Reserved ISO (55-56) - 1987 definitions
get_encoding(?RESERVED_ISO1) ->
    {ans, lllvar, 999};
get_encoding(?RESERVED_ISO2) ->
    {ans, lllvar, 999};

%% Reserved National and Private (57-64)
get_encoding(?RESERVED_NATIONAL1) ->
    {ans, lllvar, 999};
get_encoding(?RESERVED_NATIONAL2) ->
    {ans, lllvar, 999};
get_encoding(?RESERVED_NATIONAL3) ->
    {ans, lllvar, 999};
get_encoding(?RESERVED_PRIVATE1) ->
    {ans, lllvar, 999};
get_encoding(?RESERVED_PRIVATE2) ->
    {ans, lllvar, 999};
get_encoding(?RESERVED_PRIVATE3) ->
    {ans, lllvar, 999};
get_encoding(?RESERVED_PRIVATE4) ->
    {ans, lllvar, 999};
get_encoding(?MESSAGE_AUTHENTICATION_CODE) ->
    {b, fixed, 64};

%% Settlement and network management (65-73) - Field 65 is RESERVED_ISO3 in 1987
get_encoding(65) ->  % RESERVED_ISO3 in 1987
    {ans, lllvar, 999};
get_encoding(?SETTLE_CODE) ->
    {n, fixed, 1};
get_encoding(?EXTENDED_PAYMENT_CODE) ->
    {n, fixed, 2};
get_encoding(?RECEIVING_INSTITUTION_COUNTRY_CODE) ->
    {n, fixed, 3};
get_encoding(?SETTLE_INSTITUTION_COUNTRY_CODE) ->
    {n, fixed, 3};
get_encoding(?NETWORK_MANAGEMENT_INFORMATION_CODE) ->
    {n, fixed, 3};
get_encoding(?MESSAGE_NUMBER) ->
    {n, fixed, 4};
get_encoding(?MESSAGE_NUMBER_LAST) ->
    {n, fixed, 4};
get_encoding(?DATE_ACTION) ->
    {n, fixed, 6};

%% Transaction counts (74-89)
get_encoding(?CREDITS_NUMBER) ->
    {n, fixed, 10};
get_encoding(?CREDITS_REVERSAL_NUMBER) ->
    {n, fixed, 10};
get_encoding(?DEBITS_NUMBER) ->
    {n, fixed, 10};
get_encoding(?DEBITS_REVERSAL_NUMBER) ->
    {n, fixed, 10};
get_encoding(?TRANSFER_NUMBER) ->
    {n, fixed, 10};
get_encoding(?TRANSFER_NUMBER_REVERSAL) ->
    {n, fixed, 10};
get_encoding(?INQUIRIES_NUMBER) ->
    {n, fixed, 10};
get_encoding(?AUTHORIZATIONS_NUMBER) ->
    {n, fixed, 10};
get_encoding(?CREDITS_PROCESSING_FEE_AMOUNT) ->
    {n, fixed, 12};
get_encoding(?CREDITS_TRANSACTION_FEE_AMOUNT) ->
    {n, fixed, 12};
get_encoding(?DEBITS_PROCESSING_FEE_AMOUNT) ->
    {n, fixed, 12};
get_encoding(?DEBITS_TRANSACTION_FEE_AMOUNT) ->
    {n, fixed, 12};
get_encoding(?CREDITS_AMOUNT) ->
    {n, fixed, 16};
get_encoding(?CREDITS_REVERSAL_AMOUNT) ->
    {n, fixed, 16};
get_encoding(?DEBITS_AMOUNT) ->
    {n, fixed, 16};
get_encoding(?DEBITS_REVERSAL_AMOUNT) ->
    {n, fixed, 16};

%% Original data and file management (90-104)
get_encoding(?ORIGINAL_DATA_ELEMENTS) ->
    {n, fixed, 42};
get_encoding(?FILE_UPDATE_CODE) ->
    {an, fixed, 1};
get_encoding(?FILE_SECURITY_CODE) ->
    {an, fixed, 2};
get_encoding(?RESP_INDICATOR) ->
    {an, fixed, 5};
get_encoding(?SERVICE_INDICATOR) ->
    {an, fixed, 7};
get_encoding(?REPLACEMENT_AMOUNTS) ->
    {an, fixed, 42};
get_encoding(?MESSAGE_SECURITY_CODE) ->
    {b, fixed, 64};
get_encoding(?AMOUNT_NET_SETTLE) ->
    {x_n, fixed, 16};
get_encoding(?PAYEE) ->
    {ans, fixed, 25};
get_encoding(?SETTLE_INSTITUTION_ID_CODE) ->
    {n, llvar, 11};
get_encoding(?RECEIVING_INSTITUTION_ID_CODE) ->
    {n, llvar, 11};
get_encoding(?FILE_NAME) ->
    {ans, llvar, 17};
get_encoding(?ACCOUNT_ID1) ->
    {ans, llvar, 28};
get_encoding(?ACCOUNT_ID2) ->
    {ans, llvar, 28};
get_encoding(?TRAN_DESCRIPTION) ->
    {ans, lllvar, 100};

%% Private use (105-127)
get_encoding(Id) when Id >= 105 andalso Id =< 127 ->
    {ans, lllvar, 999};

%% Field 128 - Message Authentication Code 2
get_encoding(?MESSAGE_AUTHENTICATION_CODE2) ->
    {b, fixed, 64};

%% Support for list-based field IDs
get_encoding([FieldId]) when is_integer(FieldId) ->
    get_encoding(FieldId).

%% @doc Returns the name of a field given its ID.
%%
%% @spec get_field_name(integer()) -> binary()
-spec(get_field_name(integer()) -> binary()).

get_field_name(?MTI) -> <<"Message Type Indicator">>;
get_field_name(?PAN) -> <<"Primary Account Number">>;
get_field_name(?PROC_CODE) -> <<"Processing Code">>;
get_field_name(?AMOUNT_TRAN) -> <<"Transaction Amount">>;
get_field_name(?SYSTEMS_TRACE_AUDIT_NUMBER) -> <<"Systems Trace Audit Number">>;
get_field_name(?TIME_LOCAL_TRAN) -> <<"Time, Local Transaction">>;
get_field_name(?DATE_LOCAL_TRAN) -> <<"Date, Local Transaction">>;
get_field_name(?CARD_ACCEPTOR_TERMINAL_ID) -> <<"Card Acceptor Terminal ID">>;
get_field_name(?RESP_CODE) -> <<"Response Code">>;
get_field_name(Id) when Id >= 105 andalso Id =< 127 ->
    iolist_to_binary(io_lib:format("Private Use Field ~p", [Id]));
get_field_name(Id) ->
    iolist_to_binary(io_lib:format("Field ~p", [Id])).

%% @doc Checks if a field ID is valid for ISO 8583:1987.
%%
%% @spec is_valid_field(integer()) -> boolean()
-spec(is_valid_field(integer()) -> boolean()).

is_valid_field(Id) when Id >= 0 andalso Id =< 128 -> true;
is_valid_field(_) -> false.

%% @doc Returns a list of all standard field IDs.
%%
%% @spec get_all_fields() -> list(integer())
-spec(get_all_fields() -> list(integer())).

get_all_fields() ->
    lists:seq(0, 128).
