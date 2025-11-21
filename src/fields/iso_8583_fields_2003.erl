-module(iso_8583_fields_2003).

%%
%% ISO 8583:2003 Field Encoding Definitions
%%
%% This module defines field encodings for the 2003 version of ISO 8583.
%% ISO 8583:2003 introduced significant changes including:
%% - Support for up to 192 fields (with tertiary bitmap)
%% - More flexible field definitions
%% - Enhanced security fields
%%

%%
%% Include files
%%
-include("iso_8583_types.hrl").
-include("iso_8583_fields_id.hrl").

%%
%% Exported Functions
%%
-export([get_encoding/1, get_field_name/1, is_valid_field/1, get_all_fields/0]).

%%
%% API Functions
%%

%% @doc Returns how a field is encoded for ISO 8583:2003.
%%      The 2003 version maintains most 1993 fields but adds extended fields
%%      and modifies some field definitions.
%%
%% @spec get_encoding(FieldId::integer()|list(integer())) -> field_encoding()
-spec get_encoding(integer() | [integer()]) -> field_encoding().
%% Fields that changed or were added in ISO 8583:2003

%% Core fields (0-1) - Same as 1993
get_encoding(?MTI) ->
    {n, fixed, 4};
get_encoding(?BITMAP_EXTENDED) ->
    {b, fixed, 64};
%% Field 12 - Date and Time, Local Transaction - Extended format
get_encoding(?DATE_AND_TIME_LOCAL_TRAN) ->
    {n, fixed, 14};  % YYMMDDhhmmss in 2003 (extended from 12 to 14)
%% Field 22 - POS Data Code - Extended in 2003
get_encoding(?POS_DATA_CODE) ->
    {an, fixed, 12};
%% Field 43 - Card Acceptor Name/Location - Extended format
get_encoding(?CARD_ACCEPTOR_NAME_LOCATION) ->
    {ans, fixed, 99};
%% Field 48 - Additional Data - Extended usage in 2003
get_encoding(?ADDITIONAL_DATA_PRIVATE) ->
    {ans, lllvar, 999};
%% Field 53 - Security Related Control Information - Extended
get_encoding(?SECURITY_RELATED_CONTROL_INFO) ->
    {an, llvar, 48};
%% Field 54 - Additional Amounts - Extended
get_encoding(?ADDITIONAL_AMOUNTS) ->
    {an, lllvar, 255};
%% Field 55 - ICC System Related Data (EMV) - Commonly used in 2003
get_encoding(?ICC_SYSTEM_RELATED_DATA) ->
    {ans, lllvar, 999};
%% Field 62 - Private Use - Often used for invoice data
get_encoding(?RESERVED_PRIVATE2) ->
    {ans, lllvar, 999};
%% Field 63 - Private Use - Often used for network data
get_encoding(?RESERVED_PRIVATE3) ->
    {ans, lllvar, 999};
%% Field 65 - Tertiary Bitmap
get_encoding(?BITMAP_TERTIARY) ->
    {b, fixed, 64};
%% Field 96 - Message Security Code - Enhanced
get_encoding(?MESSAGE_SECURITY_CODE) ->
    {an, lllvar, 32};
%% Field 102 - Account Identification 1
get_encoding(?ACCOUNT_ID1) ->
    {ans, llvar, 28};
%% Field 103 - Account Identification 2
get_encoding(?ACCOUNT_ID2) ->
    {ans, llvar, 28};
%% Extended fields (129-192) - Available with tertiary bitmap
get_encoding(Id) when Id >= 129 andalso Id =< 192 ->
    {ans, lllvar, 999};
%% Support for list-based field IDs
get_encoding([FieldId]) when is_integer(FieldId) ->
    get_encoding(FieldId);
%% Delegate to 1993 for fields not specifically defined here
get_encoding(FieldId) when FieldId =< 128 ->
    try
        iso_8583_fields_1993:get_encoding(FieldId)
    catch
        _:_ ->
            % If field not defined in 1993, use default
            {ans, lllvar, 999}
    end;
get_encoding(_) ->
    throw(invalid_field_id).

%% @doc Returns the name of a field given its ID.
%%
%% @spec get_field_name(integer()) -> binary()
-spec get_field_name(integer()) -> binary().
%% Extended field names for 2003-specific fields
get_field_name(?DATE_AND_TIME_LOCAL_TRAN) ->
    <<"Date and Time, Local Transaction (Extended)">>;
get_field_name(?SECURITY_RELATED_CONTROL_INFO) ->
    <<"Security Related Control Information (Extended)">>;
get_field_name(?ADDITIONAL_AMOUNTS) ->
    <<"Additional Amounts (Extended)">>;
get_field_name(?MESSAGE_SECURITY_CODE) ->
    <<"Message Security Code (Enhanced)">>;
%% Extended fields (129-192)
get_field_name(Id) when Id >= 129 andalso Id =< 192 ->
    iolist_to_binary(io_lib:format("Extended Field ~p", [Id]));
%% Delegate to 1993 for standard field names
get_field_name(FieldId) when FieldId =< 128 ->
    iso_8583_fields_1993:get_field_name(FieldId);
get_field_name(Id) ->
    iolist_to_binary(io_lib:format("Field ~p", [Id])).

%% @doc Checks if a field ID is valid for ISO 8583:2003.
%%      2003 supports up to 192 fields with the tertiary bitmap.
%%
%% @spec is_valid_field(integer()) -> boolean()
-spec is_valid_field(integer()) -> boolean().
is_valid_field(Id) when Id >= 0 andalso Id =< 192 ->
    true;
is_valid_field(_) ->
    false.

%% @doc Returns a list of all standard field IDs.
%%      ISO 8583:2003 supports fields 0-192.
%%
%% @spec get_all_fields() -> list(integer())
-spec get_all_fields() -> [integer()].
get_all_fields() ->
    lists:seq(0, 192).
