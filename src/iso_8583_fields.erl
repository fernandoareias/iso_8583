-module(iso_8583_fields).

%%
%% ISO 8583 Fields Facade Module
%%
%% This module provides a unified interface for accessing field encoding definitions
%% across different ISO 8583 versions (1987, 1993, 2003).
%%
%% By default, it uses the 1993 version for backward compatibility, but you can
%% specify which version to use.
%%

%%
%% Include files
%%
-include("iso_8583_types.hrl").

%%
%% Exported Functions
%%
-export([
    % Field encoding functions
    get_encoding/1,
    get_encoding/2,

    % Field metadata
    get_field_name/1,
    get_field_name/2,
    is_valid_field/1,
    is_valid_field/2,
    get_all_fields/0,
    get_all_fields/1,

    % Version information
    get_supported_versions/0,
    get_default_version/0
]).

%%
%% Type definitions
%%
-type iso_version() :: '1987' | '1993' | '2003'.

%%
%% API Functions
%%

%% @doc Returns the field encoding using the default version (1993).
%%
%% @spec get_encoding(FieldId::integer()|list(integer())) -> field_encoding()
-spec(get_encoding(integer() | list(integer())) -> field_encoding()).

get_encoding(FieldId) ->
    get_encoding(FieldId, get_default_version()).

%% @doc Returns the field encoding for a specific ISO 8583 version.
%%
%% @spec get_encoding(FieldId::integer()|list(integer()), iso_version()) -> field_encoding()
-spec(get_encoding(integer() | list(integer()), iso_version()) -> field_encoding()).

get_encoding(FieldId, '1987') ->
    iso_8583_fields_1987:get_encoding(FieldId);
get_encoding(FieldId, '1993') ->
    iso_8583_fields_1993:get_encoding(FieldId);
get_encoding(FieldId, '2003') ->
    iso_8583_fields_2003:get_encoding(FieldId);
get_encoding(_FieldId, Version) ->
    throw({unsupported_version, Version}).

%% @doc Returns the field name using the default version (1993).
%%
%% @spec get_field_name(integer()) -> binary()
-spec(get_field_name(integer()) -> binary()).

get_field_name(FieldId) ->
    get_field_name(FieldId, get_default_version()).

%% @doc Returns the field name for a specific ISO 8583 version.
%%
%% @spec get_field_name(integer(), iso_version()) -> binary()
-spec(get_field_name(integer(), iso_version()) -> binary()).

get_field_name(FieldId, '1987') ->
    iso_8583_fields_1987:get_field_name(FieldId);
get_field_name(FieldId, '1993') ->
    iso_8583_fields_1993:get_field_name(FieldId);
get_field_name(FieldId, '2003') ->
    iso_8583_fields_2003:get_field_name(FieldId);
get_field_name(_FieldId, Version) ->
    throw({unsupported_version, Version}).

%% @doc Checks if a field ID is valid using the default version.
%%
%% @spec is_valid_field(integer()) -> boolean()
-spec(is_valid_field(integer()) -> boolean()).

is_valid_field(FieldId) ->
    is_valid_field(FieldId, get_default_version()).

%% @doc Checks if a field ID is valid for a specific ISO 8583 version.
%%
%% @spec is_valid_field(integer(), iso_version()) -> boolean()
-spec(is_valid_field(integer(), iso_version()) -> boolean()).

is_valid_field(FieldId, '1987') ->
    iso_8583_fields_1987:is_valid_field(FieldId);
is_valid_field(FieldId, '1993') ->
    iso_8583_fields_1993:is_valid_field(FieldId);
is_valid_field(FieldId, '2003') ->
    iso_8583_fields_2003:is_valid_field(FieldId);
is_valid_field(_FieldId, _Version) ->
    false.

%% @doc Returns all field IDs using the default version.
%%
%% @spec get_all_fields() -> list(integer())
-spec(get_all_fields() -> list(integer())).

get_all_fields() ->
    get_all_fields(get_default_version()).

%% @doc Returns all field IDs for a specific ISO 8583 version.
%%
%% @spec get_all_fields(iso_version()) -> list(integer())
-spec(get_all_fields(iso_version()) -> list(integer())).

get_all_fields('1987') ->
    iso_8583_fields_1987:get_all_fields();
get_all_fields('1993') ->
    iso_8583_fields_1993:get_all_fields();
get_all_fields('2003') ->
    iso_8583_fields_2003:get_all_fields();
get_all_fields(Version) ->
    throw({unsupported_version, Version}).

%% @doc Returns a list of supported ISO 8583 versions.
%%
%% @spec get_supported_versions() -> list(iso_version())
-spec(get_supported_versions() -> list(iso_version())).

get_supported_versions() ->
    ['1987', '1993', '2003'].

%% @doc Returns the default ISO 8583 version used by this module.
%%      Currently defaults to 1993 for backward compatibility.
%%
%% @spec get_default_version() -> iso_version()
-spec(get_default_version() -> iso_version()).

get_default_version() ->
    '1993'.
