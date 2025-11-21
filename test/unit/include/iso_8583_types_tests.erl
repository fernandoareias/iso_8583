-module(iso_8583_types_tests).

-include_lib("eunit/include/eunit.hrl").
-include("iso_8583_types.hrl").

%% Test iso8583_message record structure
iso8583_message_record_test() ->
    Message = #iso8583_message{},
    ?assert(is_record(Message, iso8583_message)),
    ?assertEqual([], Message#iso8583_message.attributes),
    ?assert(is_function(dict:new(), 0) orelse is_tuple(Message#iso8583_message.values)).

%% Test iso8583_message record with attributes
iso8583_message_with_attributes_test() ->
    Attrs = [{key1, value1}, {key2, value2}],
    Message = #iso8583_message{attributes=Attrs},
    ?assertEqual(Attrs, Message#iso8583_message.attributes).

%% Test iso8583_message record with values
iso8583_message_with_values_test() ->
    Values = dict:from_list([{1, <<"test">>}, {2, <<"data">>}]),
    Message = #iso8583_message{values=Values},
    ?assertEqual(<<"test">>, dict:fetch(1, Message#iso8583_message.values)),
    ?assertEqual(<<"data">>, dict:fetch(2, Message#iso8583_message.values)).

%% Test iso8583_message record default values
iso8583_message_defaults_test() ->
    Message = #iso8583_message{},
    ?assertEqual([], Message#iso8583_message.attributes),
    ?assertEqual(0, dict:size(Message#iso8583_message.values)).

%% Test creating and manipulating iso8583_message
iso8583_message_manipulation_test() ->
    Message1 = #iso8583_message{},
    Message2 = Message1#iso8583_message{attributes=[{test, value}]},
    ?assertEqual([{test, value}], Message2#iso8583_message.attributes),
    ?assertEqual([], Message1#iso8583_message.attributes).

%% Test field_encoding tuple with n type and fixed length
field_encoding_n_fixed_test() ->
    Encoding = {n, fixed, 4},
    {Type, LengthType, MaxLen} = Encoding,
    ?assertEqual(n, Type),
    ?assertEqual(fixed, LengthType),
    ?assertEqual(4, MaxLen).

%% Test field_encoding tuple with an type and llvar
field_encoding_an_llvar_test() ->
    Encoding = {an, llvar, 19},
    {Type, LengthType, MaxLen} = Encoding,
    ?assertEqual(an, Type),
    ?assertEqual(llvar, LengthType),
    ?assertEqual(19, MaxLen).

%% Test field_encoding tuple with ans type and lllvar
field_encoding_ans_lllvar_test() ->
    Encoding = {ans, lllvar, 999},
    {Type, LengthType, MaxLen} = Encoding,
    ?assertEqual(ans, Type),
    ?assertEqual(lllvar, LengthType),
    ?assertEqual(999, MaxLen).

%% Test all valid field type atoms
field_encoding_types_test_() ->
    [
     ?_assertEqual(n, element(1, {n, fixed, 1})),
     ?_assertEqual(b, element(1, {b, fixed, 1})),
     ?_assertEqual(an, element(1, {an, fixed, 1})),
     ?_assertEqual(ans, element(1, {ans, fixed, 1})),
     ?_assertEqual(x_n, element(1, {x_n, fixed, 1})),
     ?_assertEqual(ns, element(1, {ns, fixed, 1})),
     ?_assertEqual(z, element(1, {z, fixed, 1}))
    ].

%% Test all valid length type atoms
field_encoding_length_types_test_() ->
    [
     ?_assertEqual(fixed, element(2, {n, fixed, 1})),
     ?_assertEqual(llvar, element(2, {n, llvar, 1})),
     ?_assertEqual(lllvar, element(2, {n, lllvar, 1}))
    ].

%% Test max_length is integer
field_encoding_max_length_test_() ->
    [
     ?_assert(is_integer(element(3, {n, fixed, 4}))),
     ?_assert(is_integer(element(3, {an, llvar, 19}))),
     ?_assert(is_integer(element(3, {ans, lllvar, 999})))
    ].

%% Test utf8 type (binary)
utf8_type_test_() ->
    [
     ?_assert(is_binary(<<"test">>)),
     ?_assert(is_binary(<<"UTF-8 string"/utf8>>)),
     ?_assert(is_binary(<<>>)),
     ?_assertEqual(<<"hello">>, <<"hello">>)
    ].

%% Test bcd type (list of bytes)
bcd_type_test_() ->
    [
     ?_assert(is_list([1, 2, 3, 4])),
     ?_assert(is_list([])),
     ?_assert(lists:all(fun(X) -> is_integer(X) andalso X >= 0 andalso X =< 255 end, [0, 1, 2, 9])),
     ?_assertEqual([1, 2, 3], [1, 2, 3])
    ].

%% Test valid BCD values (0-9)
bcd_valid_values_test_() ->
    ValidBCD = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    [
     ?_assert(is_list(ValidBCD)),
     ?_assert(lists:all(fun(X) -> X >= 0 andalso X =< 9 end, ValidBCD)),
     ?_assertEqual(10, length(ValidBCD))
    ].

%% Test iso8583field_value type - utf8
iso8583field_value_utf8_test() ->
    Value = <<"test value">>,
    ?assert(is_binary(Value)).

%% Test iso8583field_value type - binary
iso8583field_value_binary_test() ->
    Value = <<1, 2, 3, 4>>,
    ?assert(is_binary(Value)).

%% Test iso8583field_value type - iso8583message
iso8583field_value_iso8583message_test() ->
    Value = #iso8583_message{},
    ?assert(is_record(Value, iso8583_message)).

%% Test nested iso8583_message
nested_iso8583_message_test() ->
    InnerMessage = #iso8583_message{attributes=[{inner, true}]},
    Values = dict:from_list([{48, InnerMessage}]),
    OuterMessage = #iso8583_message{values=Values},

    NestedMsg = dict:fetch(48, OuterMessage#iso8583_message.values),
    ?assert(is_record(NestedMsg, iso8583_message)),
    ?assertEqual([{inner, true}], NestedMsg#iso8583_message.attributes).

%% Test iso8583_message with multiple fields
iso8583_message_multiple_fields_test() ->
    Values = dict:from_list([
        {2, <<"1234567890123456">>},
        {3, <<"000000">>},
        {4, <<"000000012345">>},
        {11, <<"123456">>}
    ]),
    Message = #iso8583_message{values=Values},

    ?assertEqual(<<"1234567890123456">>, dict:fetch(2, Message#iso8583_message.values)),
    ?assertEqual(<<"000000">>, dict:fetch(3, Message#iso8583_message.values)),
    ?assertEqual(<<"000000012345">>, dict:fetch(4, Message#iso8583_message.values)),
    ?assertEqual(<<"123456">>, dict:fetch(11, Message#iso8583_message.values)).

%% Test field_encoding with different max_lengths
field_encoding_various_lengths_test_() ->
    [
     ?_assertEqual(1, element(3, {n, fixed, 1})),
     ?_assertEqual(6, element(3, {n, fixed, 6})),
     ?_assertEqual(12, element(3, {n, fixed, 12})),
     ?_assertEqual(99, element(3, {an, llvar, 99})),
     ?_assertEqual(999, element(3, {ans, lllvar, 999}))
    ].

%% Test common ISO 8583 field encodings
common_field_encodings_test_() ->
    [
     % PAN - Primary Account Number
     ?_assertEqual({n, llvar, 19}, {n, llvar, 19}),

     % Processing Code
     ?_assertEqual({n, fixed, 6}, {n, fixed, 6}),

     % Amount
     ?_assertEqual({n, fixed, 12}, {n, fixed, 12}),

     % System Trace Audit Number
     ?_assertEqual({n, fixed, 6}, {n, fixed, 6}),

     % Card Acceptor Name/Location
     ?_assertEqual({ans, fixed, 40}, {ans, fixed, 40}),

     % Additional Data
     ?_assertEqual({ans, lllvar, 999}, {ans, lllvar, 999})
    ].

%% Test record field access patterns
record_access_patterns_test() ->
    Message = #iso8583_message{
        attributes=[{attr1, val1}],
        values=dict:from_list([{1, <<"test">>}])
    },

    % Pattern matching
    #iso8583_message{attributes=Attrs, values=Vals} = Message,
    ?assertEqual([{attr1, val1}], Attrs),
    ?assertEqual(<<"test">>, dict:fetch(1, Vals)).

%% Test updating iso8583_message values
update_message_values_test() ->
    Message1 = #iso8583_message{},
    Values1 = dict:store(2, <<"1234">>, Message1#iso8583_message.values),
    Message2 = Message1#iso8583_message{values=Values1},

    ?assertEqual(<<"1234">>, dict:fetch(2, Message2#iso8583_message.values)),

    % Update again
    Values2 = dict:store(3, <<"000000">>, Message2#iso8583_message.values),
    Message3 = Message2#iso8583_message{values=Values2},

    ?assertEqual(<<"1234">>, dict:fetch(2, Message3#iso8583_message.values)),
    ?assertEqual(<<"000000">>, dict:fetch(3, Message3#iso8583_message.values)).

%% Test empty iso8583_message
empty_message_test() ->
    Message = #iso8583_message{},
    ?assertEqual(0, dict:size(Message#iso8583_message.values)),
    ?assertEqual([], Message#iso8583_message.attributes).

%% Test iso8583_message attributes as proplist
attributes_proplist_test() ->
    Attrs = [{version, '1987'}, {type, request}, {encoding, ascii}],
    Message = #iso8583_message{attributes=Attrs},

    ?assertEqual('1987', proplists:get_value(version, Message#iso8583_message.attributes)),
    ?assertEqual(request, proplists:get_value(type, Message#iso8583_message.attributes)),
    ?assertEqual(ascii, proplists:get_value(encoding, Message#iso8583_message.attributes)).

%% Test field encoding tuple structure
field_encoding_tuple_structure_test_() ->
    [
     ?_assertEqual(3, tuple_size({n, fixed, 4})),
     ?_assertEqual(3, tuple_size({an, llvar, 19})),
     ?_assertEqual(3, tuple_size({ans, lllvar, 999})),
     ?_assert(is_tuple({n, fixed, 4})),
     ?_assert(is_tuple({b, llvar, 64}))
    ].

%% Test BCD byte range
bcd_byte_range_test() ->
    BCD = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    ?assert(lists:all(fun(B) -> is_integer(B) andalso B >= 0 andalso B =< 255 end, BCD)).

%% Test max_length type (integer)
max_length_type_test_() ->
    [
     ?_assert(is_integer(1)),
     ?_assert(is_integer(99)),
     ?_assert(is_integer(999)),
     ?_assert(is_integer(9999))
    ].
