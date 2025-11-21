-module(iso_8583_message_utils_tests).

-include_lib("eunit/include/eunit.hrl").
-include("iso_8583_fields_id.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

create_test_message() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set(?MTI, <<"0200">>, Msg0),
    Msg2 = iso_8583:set(?PAN, <<"4111111111111111">>, Msg1),
    Msg3 = iso_8583:set(?PROC_CODE, <<"000000">>, Msg2),
    iso_8583:set(?AMOUNT_TRAN, <<"000000001000">>, Msg3).

%%====================================================================
%% MTI Type Tests
%%====================================================================

get_mti_type_request_test() ->
    Msg = iso_8583:set(?MTI, <<"0200">>, iso_8583:new()),
    ?assertEqual(request, iso_8583_message_utils:get_mti_type(Msg)).

get_mti_type_request_with_1_test() ->
    Msg = iso_8583:set(?MTI, <<"0210">>, iso_8583:new()),
    ?assertEqual(request, iso_8583_message_utils:get_mti_type(Msg)).

get_mti_type_response_test() ->
    Msg = iso_8583:set(?MTI, <<"0220">>, iso_8583:new()),
    ?assertEqual(response, iso_8583_message_utils:get_mti_type(Msg)).

get_mti_type_advice_test() ->
    Msg = iso_8583:set(?MTI, <<"0240">>, iso_8583:new()),
    ?assertEqual(advice, iso_8583_message_utils:get_mti_type(Msg)).

get_mti_type_notification_test() ->
    Msg = iso_8583:set(?MTI, <<"0260">>, iso_8583:new()),
    ?assertEqual(notification, iso_8583_message_utils:get_mti_type(Msg)).

get_mti_type_unknown_test() ->
    Msg = iso_8583:set(?MTI, <<"0290">>, iso_8583:new()),
    ?assertEqual(unknown, iso_8583_message_utils:get_mti_type(Msg)).

get_mti_type_response_3_test() ->
    Msg = iso_8583:set(?MTI, <<"0230">>, iso_8583:new()),
    ?assertEqual(response, iso_8583_message_utils:get_mti_type(Msg)).

get_mti_type_advice_5_test() ->
    Msg = iso_8583:set(?MTI, <<"0250">>, iso_8583:new()),
    ?assertEqual(advice, iso_8583_message_utils:get_mti_type(Msg)).

get_mti_type_notification_7_test() ->
    Msg = iso_8583:set(?MTI, <<"0270">>, iso_8583:new()),
    ?assertEqual(notification, iso_8583_message_utils:get_mti_type(Msg)).

%%====================================================================
%% MTI Version Tests
%%====================================================================

get_mti_version_1987_test() ->
    Msg = iso_8583:set(?MTI, <<"0200">>, iso_8583:new()),
    ?assertEqual('1987', iso_8583_message_utils:get_mti_version(Msg)).

get_mti_version_1993_test() ->
    Msg = iso_8583:set(?MTI, <<"1200">>, iso_8583:new()),
    ?assertEqual('1993', iso_8583_message_utils:get_mti_version(Msg)).

get_mti_version_2003_test() ->
    Msg = iso_8583:set(?MTI, <<"2200">>, iso_8583:new()),
    ?assertEqual('2003', iso_8583_message_utils:get_mti_version(Msg)).

get_mti_version_unknown_test() ->
    Msg = iso_8583:set(?MTI, <<"9200">>, iso_8583:new()),
    ?assertEqual(unknown, iso_8583_message_utils:get_mti_version(Msg)).

%%====================================================================
%% Message Type Check Tests
%%====================================================================

is_request_test() ->
    Msg = iso_8583:set(?MTI, <<"0200">>, iso_8583:new()),
    ?assert(iso_8583_message_utils:is_request(Msg)).

is_request_false_test() ->
    Msg = iso_8583:set(?MTI, <<"0220">>, iso_8583:new()),
    ?assertNot(iso_8583_message_utils:is_request(Msg)).

is_response_test() ->
    Msg = iso_8583:set(?MTI, <<"0220">>, iso_8583:new()),
    ?assert(iso_8583_message_utils:is_response(Msg)).

is_response_false_test() ->
    Msg = iso_8583:set(?MTI, <<"0200">>, iso_8583:new()),
    ?assertNot(iso_8583_message_utils:is_response(Msg)).

is_advice_test() ->
    Msg = iso_8583:set(?MTI, <<"0240">>, iso_8583:new()),
    ?assert(iso_8583_message_utils:is_advice(Msg)).

is_advice_false_test() ->
    Msg = iso_8583:set(?MTI, <<"0200">>, iso_8583:new()),
    ?assertNot(iso_8583_message_utils:is_advice(Msg)).

%%====================================================================
%% Repeat Flag Tests
%%====================================================================

is_repeat_false_test() ->
    Msg = iso_8583:set(?MTI, <<"0200">>, iso_8583:new()),
    ?assertNot(iso_8583_message_utils:is_repeat(Msg)).

is_repeat_true_request_test() ->
    Msg = iso_8583:set(?MTI, <<"0201">>, iso_8583:new()),
    ?assert(iso_8583_message_utils:is_repeat(Msg)).

is_repeat_true_response_test() ->
    Msg = iso_8583:set(?MTI, <<"0213">>, iso_8583:new()),
    ?assert(iso_8583_message_utils:is_repeat(Msg)).

is_repeat_true_advice_test() ->
    Msg = iso_8583:set(?MTI, <<"0225">>, iso_8583:new()),
    ?assert(iso_8583_message_utils:is_repeat(Msg)).

mark_as_repeat_request_test() ->
    Msg = iso_8583:set(?MTI, <<"0200">>, iso_8583:new()),
    Repeated = iso_8583_message_utils:mark_as_repeat(Msg),
    ?assertEqual(<<"0201">>, iso_8583:get(?MTI, Repeated)).

mark_as_repeat_response_test() ->
    Msg = iso_8583:set(?MTI, <<"0212">>, iso_8583:new()),
    Repeated = iso_8583_message_utils:mark_as_repeat(Msg),
    ?assertEqual(<<"0213">>, iso_8583:get(?MTI, Repeated)).

mark_as_repeat_advice_test() ->
    Msg = iso_8583:set(?MTI, <<"0224">>, iso_8583:new()),
    Repeated = iso_8583_message_utils:mark_as_repeat(Msg),
    ?assertEqual(<<"0225">>, iso_8583:get(?MTI, Repeated)).

mark_as_repeat_already_repeat_test() ->
    Msg = iso_8583:set(?MTI, <<"0201">>, iso_8583:new()),
    Repeated = iso_8583_message_utils:mark_as_repeat(Msg),
    ?assertEqual(<<"0201">>, iso_8583:get(?MTI, Repeated)).

mark_as_repeat_already_repeat_response_test() ->
    Msg = iso_8583:set(?MTI, <<"0213">>, iso_8583:new()),
    Repeated = iso_8583_message_utils:mark_as_repeat(Msg),
    ?assertEqual(<<"0213">>, iso_8583:get(?MTI, Repeated)).

mark_as_repeat_already_repeat_advice_test() ->
    Msg = iso_8583:set(?MTI, <<"0225">>, iso_8583:new()),
    Repeated = iso_8583_message_utils:mark_as_repeat(Msg),
    ?assertEqual(<<"0225">>, iso_8583:get(?MTI, Repeated)).

mark_as_repeat_unknown_origin_test() ->
    Msg = iso_8583:set(?MTI, <<"0206">>, iso_8583:new()),
    Repeated = iso_8583_message_utils:mark_as_repeat(Msg),
    ?assertEqual(<<"0206">>, iso_8583:get(?MTI, Repeated)).

%%====================================================================
%% Reversal Tests
%%====================================================================

is_reversal_true_test() ->
    Msg0 = iso_8583:set(?MTI, <<"0200">>, iso_8583:new()),
    Msg1 = iso_8583:set(?PROC_CODE, <<"040000">>, Msg0),
    ?assert(iso_8583_message_utils:is_reversal(Msg1)).

is_reversal_false_test() ->
    Msg0 = iso_8583:set(?MTI, <<"0200">>, iso_8583:new()),
    Msg1 = iso_8583:set(?PROC_CODE, <<"000000">>, Msg0),
    ?assertNot(iso_8583_message_utils:is_reversal(Msg1)).

is_reversal_no_proc_code_test() ->
    Msg = iso_8583:set(?MTI, <<"0200">>, iso_8583:new()),
    ?assertNot(iso_8583_message_utils:is_reversal(Msg)).

%%====================================================================
%% MTI Validation Tests
%%====================================================================

validate_mti_valid_test() ->
    Msg = iso_8583:set(?MTI, <<"0200">>, iso_8583:new()),
    ?assertEqual(ok, iso_8583_message_utils:validate_mti(Msg)).

validate_mti_no_mti_test() ->
    Msg = iso_8583:new(),
    ?assertEqual({error, invalid_mti}, iso_8583_message_utils:validate_mti(Msg)).

validate_mti_invalid_length_test() ->
    Msg = iso_8583:set(?MTI, <<"020">>, iso_8583:new()),
    ?assertEqual({error, invalid_mti}, iso_8583_message_utils:validate_mti(Msg)).

validate_mti_non_digits_test() ->
    Msg = iso_8583:set(?MTI, <<"02AB">>, iso_8583:new()),
    ?assertEqual({error, invalid_mti}, iso_8583_message_utils:validate_mti(Msg)).

validate_mti_non_digit_first_test() ->
    Msg = iso_8583:set(?MTI, <<"A200">>, iso_8583:new()),
    ?assertEqual({error, invalid_mti}, iso_8583_message_utils:validate_mti(Msg)).

validate_mti_non_digit_second_test() ->
    Msg = iso_8583:set(?MTI, <<"0B00">>, iso_8583:new()),
    ?assertEqual({error, invalid_mti}, iso_8583_message_utils:validate_mti(Msg)).

validate_mti_non_digit_third_test() ->
    Msg = iso_8583:set(?MTI, <<"02C0">>, iso_8583:new()),
    ?assertEqual({error, invalid_mti}, iso_8583_message_utils:validate_mti(Msg)).

%%====================================================================
%% Required Fields Tests
%%====================================================================

has_required_fields_true_test() ->
    Msg = create_test_message(),
    ?assert(iso_8583_message_utils:has_required_fields([?MTI, ?PAN, ?PROC_CODE], Msg)).

has_required_fields_false_test() ->
    Msg = create_test_message(),
    ?assertNot(iso_8583_message_utils:has_required_fields([?MTI, ?PAN, ?RESP_CODE], Msg)).

has_required_fields_empty_test() ->
    Msg = create_test_message(),
    ?assert(iso_8583_message_utils:has_required_fields([], Msg)).

%%====================================================================
%% Clone Fields Tests
%%====================================================================

clone_fields_all_test() ->
    Msg = create_test_message(),
    Clone = iso_8583_message_utils:clone_fields(Msg),
    ?assertEqual(iso_8583:get_fields(Msg), iso_8583:get_fields(Clone)),
    ?assertEqual(iso_8583:get(?MTI, Msg), iso_8583:get(?MTI, Clone)),
    ?assertEqual(iso_8583:get(?PAN, Msg), iso_8583:get(?PAN, Clone)).

clone_fields_specific_test() ->
    Msg = create_test_message(),
    Clone = iso_8583_message_utils:clone_fields([?MTI, ?PAN], Msg),
    ?assertEqual([?MTI, ?PAN], iso_8583:get_fields(Clone)),
    ?assertEqual(iso_8583:get(?MTI, Msg), iso_8583:get(?MTI, Clone)),
    ?assertEqual(iso_8583:get(?PAN, Msg), iso_8583:get(?PAN, Clone)).

clone_fields_to_target_test() ->
    Msg = create_test_message(),
    Target = iso_8583:set(99, <<"existing">>, iso_8583:new()),
    Clone = iso_8583_message_utils:clone_fields([?MTI], Msg, Target),
    ?assert(iso_8583:has_field(?MTI, Clone)),
    ?assert(iso_8583:has_field(99, Clone)).

clone_fields_missing_field_test() ->
    Msg = iso_8583:set(?MTI, <<"0200">>, iso_8583:new()),
    Clone = iso_8583_message_utils:clone_fields([?MTI, ?PAN], Msg),
    ?assert(iso_8583:has_field(?MTI, Clone)),
    ?assertNot(iso_8583:has_field(?PAN, Clone)).

%%====================================================================
%% Copy Fields Tests
%%====================================================================

copy_fields_test() ->
    Source = create_test_message(),
    Target = iso_8583:new(),
    Result = iso_8583_message_utils:copy_fields([?MTI, ?PAN], Source, Target),
    ?assertEqual(iso_8583:get(?MTI, Source), iso_8583:get(?MTI, Result)),
    ?assertEqual(iso_8583:get(?PAN, Source), iso_8583:get(?PAN, Result)).

%%====================================================================
%% Response Creation Tests
%%====================================================================

create_response_test() ->
    Msg = create_test_message(),
    Response = iso_8583_message_utils:create_response(Msg),
    % MTI should change from 0200 to 0210
    ?assertEqual(<<"0210">>, iso_8583:get(?MTI, Response)),
    % Other fields should be preserved
    ?assertEqual(iso_8583:get(?PAN, Msg), iso_8583:get(?PAN, Response)).

create_response_with_fields_test() ->
    Msg = create_test_message(),
    Response = iso_8583_message_utils:create_response([?MTI, ?PAN], Msg),
    ?assertEqual(<<"0210">>, iso_8583:get(?MTI, Response)),
    ?assertEqual(iso_8583:get(?PAN, Msg), iso_8583:get(?PAN, Response)),
    ?assertNot(iso_8583:has_field(?AMOUNT_TRAN, Response)).

create_response_with_code_test() ->
    Msg = create_test_message(),
    Response = iso_8583_message_utils:create_response_with_code(Msg, <<"00">>),
    ?assertEqual(<<"0210">>, iso_8583:get(?MTI, Response)),
    ?assertEqual(<<"00">>, iso_8583:get(?RESP_CODE, Response)).

create_response_with_code_and_fields_test() ->
    Msg = create_test_message(),
    Response = iso_8583_message_utils:create_response_with_code([?MTI, ?PAN], Msg, <<"05">>),
    ?assertEqual(<<"0210">>, iso_8583:get(?MTI, Response)),
    ?assertEqual(<<"05">>, iso_8583:get(?RESP_CODE, Response)),
    ?assertNot(iso_8583:has_field(?AMOUNT_TRAN, Response)).

create_response_from_response_test() ->
    Msg = iso_8583:set(?MTI, <<"0220">>, iso_8583:new()),
    Response = iso_8583_message_utils:create_response(Msg),
    ?assertEqual(<<"0230">>, iso_8583:get(?MTI, Response)).

create_response_from_advice_test() ->
    Msg = iso_8583:set(?MTI, <<"0240">>, iso_8583:new()),
    Response = iso_8583_message_utils:create_response(Msg),
    ?assertEqual(<<"0250">>, iso_8583:get(?MTI, Response)).

create_response_clears_repeat_flag_test() ->
    Msg = iso_8583:set(?MTI, <<"0201">>, iso_8583:new()),
    Response = iso_8583_message_utils:create_response(Msg),
    ?assertEqual(<<"0210">>, iso_8583:get(?MTI, Response)).

create_response_from_already_response_test() ->
    Msg = iso_8583:set(?MTI, <<"0210">>, iso_8583:new()),
    Response = iso_8583_message_utils:create_response(Msg),
    ?assertEqual(<<"0210">>, iso_8583:get(?MTI, Response)).

create_response_from_already_advice_response_test() ->
    Msg = iso_8583:set(?MTI, <<"0250">>, iso_8583:new()),
    Response = iso_8583_message_utils:create_response(Msg),
    ?assertEqual(<<"0250">>, iso_8583:get(?MTI, Response)).

create_response_unknown_type_test() ->
    Msg = iso_8583:set(?MTI, <<"0290">>, iso_8583:new()),
    Response = iso_8583_message_utils:create_response(Msg),
    ?assertEqual(<<"0290">>, iso_8583:get(?MTI, Response)).

%%====================================================================
%% Module Exports Tests
%%====================================================================

module_exports_test_() ->
    [?_assert(erlang:function_exported(iso_8583_message_utils, create_response, 1)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, create_response, 2)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, create_response_with_code, 2)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, create_response_with_code, 3)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, mark_as_repeat, 1)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, is_repeat, 1)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, clone_fields, 1)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, clone_fields, 2)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, clone_fields, 3)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, copy_fields, 3)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, get_mti_type, 1)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, get_mti_version, 1)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, is_request, 1)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, is_response, 1)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, is_advice, 1)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, is_reversal, 1)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, has_required_fields, 2)),
     ?_assert(erlang:function_exported(iso_8583_message_utils, validate_mti, 1))].
