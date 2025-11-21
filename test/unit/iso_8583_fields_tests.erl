-module(iso_8583_fields_tests).

-include_lib("eunit/include/eunit.hrl").
-include("iso_8583_types.hrl").

%%====================================================================
%% Version Information Tests
%%====================================================================

get_default_version_test() ->
    ?assertEqual('1993', iso_8583_fields:get_default_version()).

get_supported_versions_test() ->
    Versions = iso_8583_fields:get_supported_versions(),
    ?assert(lists:member('1987', Versions)),
    ?assert(lists:member('1993', Versions)),
    ?assert(lists:member('2003', Versions)).

%%====================================================================
%% Get Encoding Tests (Default Version)
%%====================================================================

get_encoding_mti_test() ->
    {n, fixed, 4} = iso_8583_fields:get_encoding(0).

get_encoding_pan_test() ->
    {n, llvar, 19} = iso_8583_fields:get_encoding(2).

get_encoding_list_id_test() ->
    {n, llvar, 19} = iso_8583_fields:get_encoding([2]).

%%====================================================================
%% Get Encoding Tests (Specific Version)
%%====================================================================

get_encoding_1987_test() ->
    {n, fixed, 4} = iso_8583_fields:get_encoding(0, '1987').

get_encoding_1993_test() ->
    {n, fixed, 4} = iso_8583_fields:get_encoding(0, '1993').

get_encoding_2003_test() ->
    {n, fixed, 4} = iso_8583_fields:get_encoding(0, '2003').

get_encoding_unsupported_version_test() ->
    ?assertException(throw, {unsupported_version, invalid_version},
                     iso_8583_fields:get_encoding(0, invalid_version)).

%%====================================================================
%% Get Field Name Tests (Default Version)
%%====================================================================

get_field_name_mti_test() ->
    ?assertEqual(<<"Message Type Indicator">>, iso_8583_fields:get_field_name(0)).

get_field_name_pan_test() ->
    ?assertEqual(<<"Primary Account Number">>, iso_8583_fields:get_field_name(2)).

%%====================================================================
%% Get Field Name Tests (Specific Version)
%%====================================================================

get_field_name_1987_test() ->
    Name = iso_8583_fields:get_field_name(0, '1987'),
    ?assert(is_binary(Name)).

get_field_name_1993_test() ->
    Name = iso_8583_fields:get_field_name(0, '1993'),
    ?assert(is_binary(Name)).

get_field_name_2003_test() ->
    Name = iso_8583_fields:get_field_name(0, '2003'),
    ?assert(is_binary(Name)).

get_field_name_unsupported_version_test() ->
    ?assertException(throw, {unsupported_version, invalid_version},
                     iso_8583_fields:get_field_name(0, invalid_version)).

%%====================================================================
%% Is Valid Field Tests (Default Version)
%%====================================================================

is_valid_field_valid_test() ->
    ?assert(iso_8583_fields:is_valid_field(0)),
    ?assert(iso_8583_fields:is_valid_field(2)),
    ?assert(iso_8583_fields:is_valid_field(64)),
    ?assert(iso_8583_fields:is_valid_field(128)).

is_valid_field_invalid_test() ->
    ?assertNot(iso_8583_fields:is_valid_field(-1)),
    ?assertNot(iso_8583_fields:is_valid_field(129)),
    ?assertNot(iso_8583_fields:is_valid_field(1000)).

%%====================================================================
%% Is Valid Field Tests (Specific Version)
%%====================================================================

is_valid_field_1987_test() ->
    ?assert(iso_8583_fields:is_valid_field(0, '1987')),
    ?assert(iso_8583_fields:is_valid_field(64, '1987')).

is_valid_field_1993_test() ->
    ?assert(iso_8583_fields:is_valid_field(0, '1993')),
    ?assert(iso_8583_fields:is_valid_field(64, '1993')).

is_valid_field_2003_test() ->
    ?assert(iso_8583_fields:is_valid_field(0, '2003')),
    ?assert(iso_8583_fields:is_valid_field(64, '2003')).

is_valid_field_invalid_version_test() ->
    ?assertNot(iso_8583_fields:is_valid_field(0, invalid_version)).

%%====================================================================
%% Get All Fields Tests
%%====================================================================

get_all_fields_default_test() ->
    Fields = iso_8583_fields:get_all_fields(),
    ?assert(is_list(Fields)),
    ?assert(lists:member(0, Fields)),
    ?assert(lists:member(2, Fields)),
    ?assert(lists:member(64, Fields)).

get_all_fields_1987_test() ->
    Fields = iso_8583_fields:get_all_fields('1987'),
    ?assert(is_list(Fields)),
    ?assert(length(Fields) > 0).

get_all_fields_1993_test() ->
    Fields = iso_8583_fields:get_all_fields('1993'),
    ?assert(is_list(Fields)),
    ?assert(length(Fields) > 0).

get_all_fields_2003_test() ->
    Fields = iso_8583_fields:get_all_fields('2003'),
    ?assert(is_list(Fields)),
    ?assert(length(Fields) > 0).

get_all_fields_unsupported_version_test() ->
    ?assertException(throw, {unsupported_version, invalid_version},
                     iso_8583_fields:get_all_fields(invalid_version)).

%%====================================================================
%% Module Exports Tests
%%====================================================================

module_exports_test_() ->
    [?_assert(erlang:function_exported(iso_8583_fields, get_encoding, 1)),
     ?_assert(erlang:function_exported(iso_8583_fields, get_encoding, 2)),
     ?_assert(erlang:function_exported(iso_8583_fields, get_field_name, 1)),
     ?_assert(erlang:function_exported(iso_8583_fields, get_field_name, 2)),
     ?_assert(erlang:function_exported(iso_8583_fields, is_valid_field, 1)),
     ?_assert(erlang:function_exported(iso_8583_fields, is_valid_field, 2)),
     ?_assert(erlang:function_exported(iso_8583_fields, get_all_fields, 0)),
     ?_assert(erlang:function_exported(iso_8583_fields, get_all_fields, 1)),
     ?_assert(erlang:function_exported(iso_8583_fields, get_supported_versions, 0)),
     ?_assert(erlang:function_exported(iso_8583_fields, get_default_version, 0))].
