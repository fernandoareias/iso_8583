-module(iso_8583_tests).

-include_lib("eunit/include/eunit.hrl").
-include("iso_8583_types.hrl").

%%====================================================================
%% Basic Message Tests
%%====================================================================

new_message_test() ->
    Msg = iso_8583:new(),
    ?assert(iso_8583:is_message(Msg)),
    ?assertEqual([], iso_8583:get_fields(Msg)).

is_message_false_test() ->
    ?assertNot(iso_8583:is_message(not_a_message)),
    ?assertNot(iso_8583:is_message(123)),
    ?assertNot(iso_8583:is_message(<<"binary">>)).

%%====================================================================
%% Set/Get Field Tests
%%====================================================================

set_get_field_binary_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set(2, <<"4111111111111111">>, Msg0),
    ?assertEqual(<<"4111111111111111">>, iso_8583:get(2, Msg1)).

set_get_field_string_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set(2, "4111111111111111", Msg0),
    ?assertEqual(<<"4111111111111111">>, iso_8583:get(2, Msg1)).

set_get_field_list_id_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set([2], <<"value">>, Msg0),
    ?assertEqual(<<"value">>, iso_8583:get([2], Msg1)).

set_get_nested_field_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set([127, 2], <<"nested_value">>, Msg0),
    ?assertEqual(<<"nested_value">>, iso_8583:get([127, 2], Msg1)).

set_overwrite_field_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set(2, <<"old_value">>, Msg0),
    Msg2 = iso_8583:set(2, <<"new_value">>, Msg1),
    ?assertEqual(<<"new_value">>, iso_8583:get(2, Msg2)).

%%====================================================================
%% Get with Default Tests
%%====================================================================

get_with_default_exists_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set(2, <<"value">>, Msg0),
    ?assertEqual(<<"value">>, iso_8583:get(2, Msg1, <<"default">>)).

get_with_default_not_exists_test() ->
    Msg = iso_8583:new(),
    ?assertEqual(<<"default">>, iso_8583:get(2, Msg, <<"default">>)).

get_with_default_list_id_test() ->
    Msg = iso_8583:new(),
    ?assertEqual(<<"default">>, iso_8583:get([2], Msg, <<"default">>)).

get_with_default_nested_not_exists_test() ->
    Msg = iso_8583:new(),
    ?assertEqual(<<"default">>, iso_8583:get([127, 2], Msg, <<"default">>)).

get_with_default_nested_exists_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set([127, 2], <<"nested">>, Msg0),
    ?assertEqual(<<"nested">>, iso_8583:get([127, 2], Msg1, <<"default">>)).

%%====================================================================
%% Has Field Tests
%%====================================================================

has_field_true_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set(2, <<"value">>, Msg0),
    ?assert(iso_8583:has_field(2, Msg1)).

has_field_false_test() ->
    Msg = iso_8583:new(),
    ?assertNot(iso_8583:has_field(2, Msg)).

has_field_list_id_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set([2], <<"value">>, Msg0),
    ?assert(iso_8583:has_field([2], Msg1)).

has_field_nested_true_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set([127, 2], <<"nested">>, Msg0),
    ?assert(iso_8583:has_field([127, 2], Msg1)).

has_field_nested_false_test() ->
    Msg = iso_8583:new(),
    ?assertNot(iso_8583:has_field([127, 2], Msg)).

%%====================================================================
%% Get Fields Tests
%%====================================================================

get_fields_empty_test() ->
    Msg = iso_8583:new(),
    ?assertEqual([], iso_8583:get_fields(Msg)).

get_fields_multiple_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set(2, <<"pan">>, Msg0),
    Msg2 = iso_8583:set(0, <<"0200">>, Msg1),
    Msg3 = iso_8583:set(4, <<"amount">>, Msg2),
    ?assertEqual([0, 2, 4], iso_8583:get_fields(Msg3)).

%%====================================================================
%% Remove Field Tests
%%====================================================================

remove_field_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set(2, <<"value">>, Msg0),
    Msg2 = iso_8583:remove(2, Msg1),
    ?assertNot(iso_8583:has_field(2, Msg2)).

remove_field_list_id_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set([2], <<"value">>, Msg0),
    Msg2 = iso_8583:remove([2], Msg1),
    ?assertNot(iso_8583:has_field(2, Msg2)).

remove_nested_field_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set([127, 2], <<"nested1">>, Msg0),
    Msg2 = iso_8583:set([127, 3], <<"nested2">>, Msg1),
    Msg3 = iso_8583:remove([127, 2], Msg2),
    ?assertNot(iso_8583:has_field([127, 2], Msg3)),
    ?assert(iso_8583:has_field([127, 3], Msg3)).

remove_nested_field_removes_parent_if_empty_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set([127, 2], <<"nested">>, Msg0),
    Msg2 = iso_8583:remove([127, 2], Msg1),
    ?assertNot(iso_8583:has_field(127, Msg2)).

remove_nonexistent_nested_test() ->
    Msg = iso_8583:new(),
    Msg2 = iso_8583:remove([127, 2], Msg),
    ?assertEqual([], iso_8583:get_fields(Msg2)).

%%====================================================================
%% MTI Tests
%%====================================================================

set_get_mti_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set_mti("0200", Msg0),
    ?assertEqual(<<"0200">>, iso_8583:get_mti(Msg1)).

%%====================================================================
%% Attribute Tests
%%====================================================================

set_get_attribute_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set_attribute(my_key, my_value, Msg0),
    ?assertEqual(my_value, iso_8583:get_attribute(my_key, Msg1)).

get_attribute_not_found_test() ->
    Msg = iso_8583:new(),
    ?assertException(throw, {attribute_not_found, missing_key},
                     iso_8583:get_attribute(missing_key, Msg)).

get_attribute_with_default_exists_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set_attribute(my_key, my_value, Msg0),
    ?assertEqual(my_value, iso_8583:get_attribute(my_key, Msg1, default_value)).

get_attribute_with_default_not_exists_test() ->
    Msg = iso_8583:new(),
    ?assertEqual(default_value, iso_8583:get_attribute(missing_key, Msg, default_value)).

get_attribute_keys_empty_test() ->
    Msg = iso_8583:new(),
    ?assertEqual([], iso_8583:get_attribute_keys(Msg)).

get_attribute_keys_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set_attribute(key1, value1, Msg0),
    Msg2 = iso_8583:set_attribute(key2, value2, Msg1),
    Keys = iso_8583:get_attribute_keys(Msg2),
    ?assert(lists:member(key1, Keys)),
    ?assert(lists:member(key2, Keys)).

remove_attribute_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set_attribute(my_key, my_value, Msg0),
    Msg2 = iso_8583:remove_attribute(my_key, Msg1),
    ?assertEqual(default, iso_8583:get_attribute(my_key, Msg2, default)).

set_attribute_overwrites_test() ->
    Msg0 = iso_8583:new(),
    Msg1 = iso_8583:set_attribute(my_key, old_value, Msg0),
    Msg2 = iso_8583:set_attribute(my_key, new_value, Msg1),
    ?assertEqual(new_value, iso_8583:get_attribute(my_key, Msg2)).

%%====================================================================
%% Nested Message Tests
%%====================================================================

set_nested_message_test() ->
    Msg0 = iso_8583:new(),
    SubMsg0 = iso_8583:new(),
    SubMsg1 = iso_8583:set(2, <<"sub_value">>, SubMsg0),
    Msg1 = iso_8583:set(127, SubMsg1, Msg0),
    Retrieved = iso_8583:get(127, Msg1),
    ?assert(iso_8583:is_message(Retrieved)),
    ?assertEqual(<<"sub_value">>, iso_8583:get(2, Retrieved)).

%%====================================================================
%% Module Exports Tests
%%====================================================================

module_exports_test_() ->
    [?_assert(erlang:function_exported(iso_8583, new, 0)),
     ?_assert(erlang:function_exported(iso_8583, set, 3)),
     ?_assert(erlang:function_exported(iso_8583, get, 2)),
     ?_assert(erlang:function_exported(iso_8583, get, 3)),
     ?_assert(erlang:function_exported(iso_8583, has_field, 2)),
     ?_assert(erlang:function_exported(iso_8583, get_fields, 1)),
     ?_assert(erlang:function_exported(iso_8583, remove, 2)),
     ?_assert(erlang:function_exported(iso_8583, set_mti, 2)),
     ?_assert(erlang:function_exported(iso_8583, get_mti, 1)),
     ?_assert(erlang:function_exported(iso_8583, is_message, 1)),
     ?_assert(erlang:function_exported(iso_8583, set_attribute, 3)),
     ?_assert(erlang:function_exported(iso_8583, get_attribute, 2)),
     ?_assert(erlang:function_exported(iso_8583, get_attribute, 3)),
     ?_assert(erlang:function_exported(iso_8583, get_attribute_keys, 1)),
     ?_assert(erlang:function_exported(iso_8583, remove_attribute, 2))].
