-module(iso_8583_marshallers_tests).

-include_lib("eunit/include/eunit.hrl").

-include("iso_8583_marshallers.hrl").

%% Test MARSHALLER_ASCII definition
marshaller_ascii_test_() ->
    [?_assert(is_list(?MARSHALLER_ASCII)),
     ?_assertEqual(4, length(?MARSHALLER_ASCII)),
     ?_assertEqual(iso_8583_marshaller_ascii,
                   proplists:get_value(field_marshaller, ?MARSHALLER_ASCII)),
     ?_assertEqual(iso_8583_marshaller_ascii,
                   proplists:get_value(bitmap_marshaller, ?MARSHALLER_ASCII)),
     ?_assertEqual(iso_8583_marshaller_ascii,
                   proplists:get_value(mti_marshaller, ?MARSHALLER_ASCII)),
     ?_assertEqual(iso_8583_marshaller_ascii,
                   proplists:get_value(end_marshaller, ?MARSHALLER_ASCII))].

%% Test MARSHALLER_BINARY definition
marshaller_binary_test_() ->
    [?_assert(is_list(?MARSHALLER_BINARY)),
     ?_assertEqual(4, length(?MARSHALLER_BINARY)),
     ?_assertEqual(iso_8583_marshaller_binary,
                   proplists:get_value(field_marshaller, ?MARSHALLER_BINARY)),
     ?_assertEqual(iso_8583_marshaller_binary,
                   proplists:get_value(bitmap_marshaller, ?MARSHALLER_BINARY)),
     ?_assertEqual(iso_8583_marshaller_binary,
                   proplists:get_value(mti_marshaller, ?MARSHALLER_BINARY)),
     ?_assertEqual(iso_8583_marshaller_binary,
                   proplists:get_value(end_marshaller, ?MARSHALLER_BINARY))].

%% Test MARSHALLER_EBCDIC definition
marshaller_ebcdic_test_() ->
    [?_assert(is_list(?MARSHALLER_EBCDIC)),
     ?_assertEqual(4, length(?MARSHALLER_EBCDIC)),
     ?_assertEqual(iso_8583_marshaller_ebcdic,
                   proplists:get_value(field_marshaller, ?MARSHALLER_EBCDIC)),
     ?_assertEqual(iso_8583_marshaller_ebcdic,
                   proplists:get_value(bitmap_marshaller, ?MARSHALLER_EBCDIC)),
     ?_assertEqual(iso_8583_marshaller_ebcdic,
                   proplists:get_value(mti_marshaller, ?MARSHALLER_EBCDIC)),
     ?_assertEqual(iso_8583_marshaller_ebcdic,
                   proplists:get_value(end_marshaller, ?MARSHALLER_EBCDIC))].

%% Test MARSHALLER_JSON definition
marshaller_json_test_() ->
    [?_assert(is_list(?MARSHALLER_JSON)),
     ?_assertEqual(5, length(?MARSHALLER_JSON)),
     ?_assertEqual(iso_8583_marshaller_json,
                   proplists:get_value(field_marshaller, ?MARSHALLER_JSON)),
     ?_assertEqual(iso_8583_marshaller_json,
                   proplists:get_value(bitmap_marshaller, ?MARSHALLER_JSON)),
     ?_assertEqual(iso_8583_marshaller_json,
                   proplists:get_value(mti_marshaller, ?MARSHALLER_JSON)),
     ?_assertEqual(iso_8583_marshaller_json,
                   proplists:get_value(init_marshaller, ?MARSHALLER_JSON)),
     ?_assertEqual(iso_8583_marshaller_json,
                   proplists:get_value(end_marshaller, ?MARSHALLER_JSON))].

%% Test MARSHALLER_XML definition
marshaller_xml_test_() ->
    [?_assert(is_list(?MARSHALLER_XML)),
     ?_assertEqual(5, length(?MARSHALLER_XML)),
     ?_assertEqual(iso_8583_marshaller_xml,
                   proplists:get_value(field_marshaller, ?MARSHALLER_XML)),
     ?_assertEqual(iso_8583_marshaller_xml,
                   proplists:get_value(bitmap_marshaller, ?MARSHALLER_XML)),
     ?_assertEqual(iso_8583_marshaller_xml,
                   proplists:get_value(mti_marshaller, ?MARSHALLER_XML)),
     ?_assertEqual(iso_8583_marshaller_xml,
                   proplists:get_value(init_marshaller, ?MARSHALLER_XML)),
     ?_assertEqual(iso_8583_marshaller_xml,
                   proplists:get_value(end_marshaller, ?MARSHALLER_XML))].

%% Test MARSHALLER_GRPC definition
marshaller_grpc_test_() ->
    [?_assert(is_list(?MARSHALLER_GRPC)),
     ?_assertEqual(4, length(?MARSHALLER_GRPC)),
     ?_assertEqual(iso_8583_marshaller_grpc,
                   proplists:get_value(field_marshaller, ?MARSHALLER_GRPC)),
     ?_assertEqual(iso_8583_marshaller_grpc,
                   proplists:get_value(bitmap_marshaller, ?MARSHALLER_GRPC)),
     ?_assertEqual(iso_8583_marshaller_grpc,
                   proplists:get_value(mti_marshaller, ?MARSHALLER_GRPC)),
     ?_assertEqual(iso_8583_marshaller_grpc,
                   proplists:get_value(message_marshaller, ?MARSHALLER_GRPC))].

%% Test that all basic marshallers (ASCII, BINARY, EBCDIC) have the same structure
basic_marshallers_structure_test_() ->
    [?_assertEqual(length(?MARSHALLER_ASCII), length(?MARSHALLER_BINARY)),
     ?_assertEqual(length(?MARSHALLER_ASCII), length(?MARSHALLER_EBCDIC)),
     ?_assert(proplists:is_defined(field_marshaller, ?MARSHALLER_ASCII)),
     ?_assert(proplists:is_defined(field_marshaller, ?MARSHALLER_BINARY)),
     ?_assert(proplists:is_defined(field_marshaller, ?MARSHALLER_EBCDIC)),
     ?_assert(proplists:is_defined(bitmap_marshaller, ?MARSHALLER_ASCII)),
     ?_assert(proplists:is_defined(bitmap_marshaller, ?MARSHALLER_BINARY)),
     ?_assert(proplists:is_defined(bitmap_marshaller, ?MARSHALLER_EBCDIC)),
     ?_assert(proplists:is_defined(mti_marshaller, ?MARSHALLER_ASCII)),
     ?_assert(proplists:is_defined(mti_marshaller, ?MARSHALLER_BINARY)),
     ?_assert(proplists:is_defined(mti_marshaller, ?MARSHALLER_EBCDIC)),
     ?_assert(proplists:is_defined(end_marshaller, ?MARSHALLER_ASCII)),
     ?_assert(proplists:is_defined(end_marshaller, ?MARSHALLER_BINARY)),
     ?_assert(proplists:is_defined(end_marshaller, ?MARSHALLER_EBCDIC))].

%% Test that JSON and XML marshallers have init_marshaller
json_xml_init_marshaller_test_() ->
    [?_assert(proplists:is_defined(init_marshaller, ?MARSHALLER_JSON)),
     ?_assert(proplists:is_defined(init_marshaller, ?MARSHALLER_XML)),
     ?_assertEqual(undefined, proplists:get_value(init_marshaller, ?MARSHALLER_ASCII)),
     ?_assertEqual(undefined, proplists:get_value(init_marshaller, ?MARSHALLER_BINARY)),
     ?_assertEqual(undefined, proplists:get_value(init_marshaller, ?MARSHALLER_EBCDIC)),
     ?_assertEqual(undefined, proplists:get_value(init_marshaller, ?MARSHALLER_GRPC))].

%% Test that GRPC marshaller has message_marshaller instead of end_marshaller
grpc_marshaller_structure_test_() ->
    [?_assert(proplists:is_defined(message_marshaller, ?MARSHALLER_GRPC)),
     ?_assertEqual(undefined, proplists:get_value(end_marshaller, ?MARSHALLER_GRPC)),
     ?_assertEqual(undefined, proplists:get_value(init_marshaller, ?MARSHALLER_GRPC))].

%% Test all marshallers have required field_marshaller
all_have_field_marshaller_test_() ->
    [?_assert(proplists:is_defined(field_marshaller, ?MARSHALLER_ASCII)),
     ?_assert(proplists:is_defined(field_marshaller, ?MARSHALLER_BINARY)),
     ?_assert(proplists:is_defined(field_marshaller, ?MARSHALLER_EBCDIC)),
     ?_assert(proplists:is_defined(field_marshaller, ?MARSHALLER_JSON)),
     ?_assert(proplists:is_defined(field_marshaller, ?MARSHALLER_XML)),
     ?_assert(proplists:is_defined(field_marshaller, ?MARSHALLER_GRPC))].

%% Test all marshallers have bitmap_marshaller
all_have_bitmap_marshaller_test_() ->
    [?_assert(proplists:is_defined(bitmap_marshaller, ?MARSHALLER_ASCII)),
     ?_assert(proplists:is_defined(bitmap_marshaller, ?MARSHALLER_BINARY)),
     ?_assert(proplists:is_defined(bitmap_marshaller, ?MARSHALLER_EBCDIC)),
     ?_assert(proplists:is_defined(bitmap_marshaller, ?MARSHALLER_JSON)),
     ?_assert(proplists:is_defined(bitmap_marshaller, ?MARSHALLER_XML)),
     ?_assert(proplists:is_defined(bitmap_marshaller, ?MARSHALLER_GRPC))].

%% Test all marshallers have mti_marshaller
all_have_mti_marshaller_test_() ->
    [?_assert(proplists:is_defined(mti_marshaller, ?MARSHALLER_ASCII)),
     ?_assert(proplists:is_defined(mti_marshaller, ?MARSHALLER_BINARY)),
     ?_assert(proplists:is_defined(mti_marshaller, ?MARSHALLER_EBCDIC)),
     ?_assert(proplists:is_defined(mti_marshaller, ?MARSHALLER_JSON)),
     ?_assert(proplists:is_defined(mti_marshaller, ?MARSHALLER_XML)),
     ?_assert(proplists:is_defined(mti_marshaller, ?MARSHALLER_GRPC))].

%% Test marshaller consistency - same module for all components in each marshaller
marshaller_consistency_ascii_test() ->
    FieldMarshaller = proplists:get_value(field_marshaller, ?MARSHALLER_ASCII),
    BitmapMarshaller = proplists:get_value(bitmap_marshaller, ?MARSHALLER_ASCII),
    MtiMarshaller = proplists:get_value(mti_marshaller, ?MARSHALLER_ASCII),
    EndMarshaller = proplists:get_value(end_marshaller, ?MARSHALLER_ASCII),
    ?assertEqual(FieldMarshaller, BitmapMarshaller),
    ?assertEqual(FieldMarshaller, MtiMarshaller),
    ?assertEqual(FieldMarshaller, EndMarshaller).

marshaller_consistency_binary_test() ->
    FieldMarshaller = proplists:get_value(field_marshaller, ?MARSHALLER_BINARY),
    BitmapMarshaller = proplists:get_value(bitmap_marshaller, ?MARSHALLER_BINARY),
    MtiMarshaller = proplists:get_value(mti_marshaller, ?MARSHALLER_BINARY),
    EndMarshaller = proplists:get_value(end_marshaller, ?MARSHALLER_BINARY),
    ?assertEqual(FieldMarshaller, BitmapMarshaller),
    ?assertEqual(FieldMarshaller, MtiMarshaller),
    ?assertEqual(FieldMarshaller, EndMarshaller).

marshaller_consistency_ebcdic_test() ->
    FieldMarshaller = proplists:get_value(field_marshaller, ?MARSHALLER_EBCDIC),
    BitmapMarshaller = proplists:get_value(bitmap_marshaller, ?MARSHALLER_EBCDIC),
    MtiMarshaller = proplists:get_value(mti_marshaller, ?MARSHALLER_EBCDIC),
    EndMarshaller = proplists:get_value(end_marshaller, ?MARSHALLER_EBCDIC),
    ?assertEqual(FieldMarshaller, BitmapMarshaller),
    ?assertEqual(FieldMarshaller, MtiMarshaller),
    ?assertEqual(FieldMarshaller, EndMarshaller).

marshaller_consistency_json_test() ->
    FieldMarshaller = proplists:get_value(field_marshaller, ?MARSHALLER_JSON),
    BitmapMarshaller = proplists:get_value(bitmap_marshaller, ?MARSHALLER_JSON),
    MtiMarshaller = proplists:get_value(mti_marshaller, ?MARSHALLER_JSON),
    InitMarshaller = proplists:get_value(init_marshaller, ?MARSHALLER_JSON),
    EndMarshaller = proplists:get_value(end_marshaller, ?MARSHALLER_JSON),
    ?assertEqual(FieldMarshaller, BitmapMarshaller),
    ?assertEqual(FieldMarshaller, MtiMarshaller),
    ?assertEqual(FieldMarshaller, InitMarshaller),
    ?assertEqual(FieldMarshaller, EndMarshaller).

marshaller_consistency_xml_test() ->
    FieldMarshaller = proplists:get_value(field_marshaller, ?MARSHALLER_XML),
    BitmapMarshaller = proplists:get_value(bitmap_marshaller, ?MARSHALLER_XML),
    MtiMarshaller = proplists:get_value(mti_marshaller, ?MARSHALLER_XML),
    InitMarshaller = proplists:get_value(init_marshaller, ?MARSHALLER_XML),
    EndMarshaller = proplists:get_value(end_marshaller, ?MARSHALLER_XML),
    ?assertEqual(FieldMarshaller, BitmapMarshaller),
    ?assertEqual(FieldMarshaller, MtiMarshaller),
    ?assertEqual(FieldMarshaller, InitMarshaller),
    ?assertEqual(FieldMarshaller, EndMarshaller).

marshaller_consistency_grpc_test() ->
    FieldMarshaller = proplists:get_value(field_marshaller, ?MARSHALLER_GRPC),
    BitmapMarshaller = proplists:get_value(bitmap_marshaller, ?MARSHALLER_GRPC),
    MtiMarshaller = proplists:get_value(mti_marshaller, ?MARSHALLER_GRPC),
    MessageMarshaller = proplists:get_value(message_marshaller, ?MARSHALLER_GRPC),
    ?assertEqual(FieldMarshaller, BitmapMarshaller),
    ?assertEqual(FieldMarshaller, MtiMarshaller),
    ?assertEqual(FieldMarshaller, MessageMarshaller).

%% Test that all marshallers are different from each other
marshallers_are_different_test_() ->
    [?_assert(?MARSHALLER_ASCII =/= ?MARSHALLER_BINARY),
     ?_assert(?MARSHALLER_ASCII =/= ?MARSHALLER_EBCDIC),
     ?_assert(?MARSHALLER_ASCII =/= ?MARSHALLER_JSON),
     ?_assert(?MARSHALLER_ASCII =/= ?MARSHALLER_XML),
     ?_assert(?MARSHALLER_ASCII =/= ?MARSHALLER_GRPC),
     ?_assert(?MARSHALLER_BINARY =/= ?MARSHALLER_EBCDIC),
     ?_assert(?MARSHALLER_BINARY =/= ?MARSHALLER_JSON),
     ?_assert(?MARSHALLER_BINARY =/= ?MARSHALLER_XML),
     ?_assert(?MARSHALLER_BINARY =/= ?MARSHALLER_GRPC),
     ?_assert(?MARSHALLER_EBCDIC =/= ?MARSHALLER_JSON),
     ?_assert(?MARSHALLER_EBCDIC =/= ?MARSHALLER_XML),
     ?_assert(?MARSHALLER_EBCDIC =/= ?MARSHALLER_GRPC),
     ?_assert(?MARSHALLER_JSON =/= ?MARSHALLER_XML),
     ?_assert(?MARSHALLER_JSON =/= ?MARSHALLER_GRPC),
     ?_assert(?MARSHALLER_XML =/= ?MARSHALLER_GRPC)].

%% Test marshaller module names are atoms
marshaller_module_names_are_atoms_test_() ->
    [?_assert(is_atom(proplists:get_value(field_marshaller, ?MARSHALLER_ASCII))),
     ?_assert(is_atom(proplists:get_value(field_marshaller, ?MARSHALLER_BINARY))),
     ?_assert(is_atom(proplists:get_value(field_marshaller, ?MARSHALLER_EBCDIC))),
     ?_assert(is_atom(proplists:get_value(field_marshaller, ?MARSHALLER_JSON))),
     ?_assert(is_atom(proplists:get_value(field_marshaller, ?MARSHALLER_XML))),
     ?_assert(is_atom(proplists:get_value(field_marshaller, ?MARSHALLER_GRPC)))].

%% Test proplists format
marshallers_are_valid_proplists_test_() ->
    [?_assert(is_list(?MARSHALLER_ASCII)),
     ?_assert(is_list(?MARSHALLER_BINARY)),
     ?_assert(is_list(?MARSHALLER_EBCDIC)),
     ?_assert(is_list(?MARSHALLER_JSON)),
     ?_assert(is_list(?MARSHALLER_XML)),
     ?_assert(is_list(?MARSHALLER_GRPC)),
     %% Verify all elements are tuples
     ?_assert(lists:all(fun(E) -> is_tuple(E) end, ?MARSHALLER_ASCII)),
     ?_assert(lists:all(fun(E) -> is_tuple(E) end, ?MARSHALLER_BINARY)),
     ?_assert(lists:all(fun(E) -> is_tuple(E) end, ?MARSHALLER_EBCDIC)),
     ?_assert(lists:all(fun(E) -> is_tuple(E) end, ?MARSHALLER_JSON)),
     ?_assert(lists:all(fun(E) -> is_tuple(E) end, ?MARSHALLER_XML)),
     ?_assert(lists:all(fun(E) -> is_tuple(E) end, ?MARSHALLER_GRPC))].

%% Test that marshaller keys are atoms
marshaller_keys_are_atoms_test_() ->
    CheckKeys = fun(Marshaller) -> lists:all(fun({K, _V}) -> is_atom(K) end, Marshaller) end,
    [?_assert(CheckKeys(?MARSHALLER_ASCII)),
     ?_assert(CheckKeys(?MARSHALLER_BINARY)),
     ?_assert(CheckKeys(?MARSHALLER_EBCDIC)),
     ?_assert(CheckKeys(?MARSHALLER_JSON)),
     ?_assert(CheckKeys(?MARSHALLER_XML)),
     ?_assert(CheckKeys(?MARSHALLER_GRPC))].

%% Test that marshaller values are atoms
marshaller_values_are_atoms_test_() ->
    CheckValues =
        fun(Marshaller) -> lists:all(fun({_K, V}) -> is_atom(V) end, Marshaller) end,
    [?_assert(CheckValues(?MARSHALLER_ASCII)),
     ?_assert(CheckValues(?MARSHALLER_BINARY)),
     ?_assert(CheckValues(?MARSHALLER_EBCDIC)),
     ?_assert(CheckValues(?MARSHALLER_JSON)),
     ?_assert(CheckValues(?MARSHALLER_XML)),
     ?_assert(CheckValues(?MARSHALLER_GRPC))].
