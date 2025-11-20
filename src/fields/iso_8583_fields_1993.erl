-module(iso_8583_fields_1993).

%%
%% ISO 8583:1993 Field Encoding Definitions
%%
%% This module defines field encodings for the 1993 version of ISO 8583.
%% It inherits most fields from the 1987 version but overrides specific fields
%% that changed in the 1993 specification.
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

%% @doc Returns how a field is encoded. This version overrides fields that changed
%%      from 1987 to 1993 and delegates to 1987 for unchanged fields.
%%
%% @spec get_encoding(FieldId::integer()|list(integer())) -> field_encoding()
-spec(get_encoding(integer() | list(integer())) -> field_encoding()).

%% Fields that changed in ISO 8583:1993

%% Field 12 - Changed from "Time, Local Transaction" to "Date and Time, Local Transaction"
get_encoding(?DATE_AND_TIME_LOCAL_TRAN) ->
    {n, fixed, 12};

%% Field 13 - Changed from "Date, Local Transaction" to "Date, Effective"
get_encoding(?DATE_EFF) ->
    {n, fixed, 4};

%% Field 15 - Date, Settlement - Length changed
get_encoding(?DATE_SETTLE) ->
    {n, fixed, 6};

%% Field 22 - Changed from "POS Entry Mode" to "POS Data Code"
get_encoding(?POS_DATA_CODE) ->
    {an, fixed, 12};

%% Field 24 - Changed from "Network International ID" to "Function Code"
get_encoding(?FUNCTION_CODE) ->
    {n, fixed, 3};

%% Field 25 - Changed from "POS Condition Code" to "Message Reason Code"
get_encoding(?MESSAGE_REASON_CODE) ->
    {n, fixed, 4};

%% Field 26 - Changed from "POS Capture Code" to "Card Acceptor Business Code"
get_encoding(?CARD_ACCEPTOR_BUSINESS_CODE) ->
    {n, fixed, 4};

%% Field 30 - Changed from "Amount, Transaction Processing Fee" to "Amount, Original"
get_encoding(?AMOUNT_ORIGINAL) ->
    {n, fixed, 24};

%% Field 31 - Changed from "Amount, Settlement Processing Fee" to "Acquirer Reference Data"
get_encoding(?ACQUIRER_REFERENCE_DATA) ->
    {n, llvar, 48};

%% Field 39 - Response Code - Length changed
get_encoding(?RESP_CODE) ->
    {an, fixed, 3};

%% Field 43 - Card Acceptor Name/Location - Format changed
get_encoding(?CARD_ACCEPTOR_NAME_LOCATION) ->
    {an, llvar, 99};

%% Field 53 - Security Related Control Information - Format changed
get_encoding(?SECURITY_RELATED_CONTROL_INFO) ->
    {an, llvar, 8};

%% Field 55 - Changed from "Reserved ISO 1" to "ICC System Related Data" (EMV)
get_encoding(?ICC_SYSTEM_RELATED_DATA) ->
    {ans, lllvar, 999};

%% Field 56 - Changed from "Reserved ISO 2" to "Original Data Elements"
get_encoding(?ORIGINAL_DATA_ELEMENTS_1993) ->
    {an, llvar, 35};

%% Field 65 - Changed from "Reserved ISO 3" to "Bitmap, Tertiary" (for extended fields)
get_encoding(?BITMAP_TERTIARY) ->
    {b, fixed, 64};

%% Field 96 - Message Security Code - Format changed
get_encoding(?MESSAGE_SECURITY_CODE) ->
    {an, llvar, 18};

%% Support for list-based field IDs
get_encoding([FieldId]) when is_integer(FieldId) ->
    get_encoding(FieldId);

%% Delegate to 1987 for all other fields
get_encoding(FieldId) ->
    iso_8583_fields_1987:get_encoding(FieldId).

%% @doc Returns the name of a field given its ID.
%%
%% @spec get_field_name(integer()) -> binary()
-spec(get_field_name(integer()) -> binary()).

%% Override field names that changed in 1993
get_field_name(?DATE_AND_TIME_LOCAL_TRAN) -> <<"Date and Time, Local Transaction">>;
get_field_name(?DATE_EFF) -> <<"Date, Effective">>;
get_field_name(?POS_DATA_CODE) -> <<"POS Data Code">>;
get_field_name(?FUNCTION_CODE) -> <<"Function Code">>;
get_field_name(?MESSAGE_REASON_CODE) -> <<"Message Reason Code">>;
get_field_name(?CARD_ACCEPTOR_BUSINESS_CODE) -> <<"Card Acceptor Business Code">>;
get_field_name(?AMOUNT_ORIGINAL) -> <<"Amount, Original">>;
get_field_name(?ACQUIRER_REFERENCE_DATA) -> <<"Acquirer Reference Data">>;
get_field_name(?ICC_SYSTEM_RELATED_DATA) -> <<"ICC System Related Data">>;
get_field_name(?ORIGINAL_DATA_ELEMENTS_1993) -> <<"Original Data Elements">>;
get_field_name(?BITMAP_TERTIARY) -> <<"Bitmap, Tertiary">>;

%% Delegate to 1987 for other field names
get_field_name(FieldId) ->
    iso_8583_fields_1987:get_field_name(FieldId).

%% @doc Checks if a field ID is valid for ISO 8583:1993.
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
