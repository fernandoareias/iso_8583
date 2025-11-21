-module(iso_8583_marshaller_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Get Marshaller Config Tests
%%====================================================================

get_marshaller_config_ascii_test() ->
    Config = iso_8583_marshaller:get_marshaller_config(ascii),
    ?assert(is_list(Config)).

get_marshaller_config_binary_test() ->
    Config = iso_8583_marshaller:get_marshaller_config(binary),
    ?assert(is_list(Config)).

get_marshaller_config_ebcdic_test() ->
    Config = iso_8583_marshaller:get_marshaller_config(ebcdic),
    ?assert(is_list(Config)).

get_marshaller_config_json_test() ->
    Config = iso_8583_marshaller:get_marshaller_config(json),
    ?assert(is_list(Config)).

get_marshaller_config_xml_test() ->
    Config = iso_8583_marshaller:get_marshaller_config(xml),
    ?assert(is_list(Config)).

get_marshaller_config_grpc_test() ->
    Config = iso_8583_marshaller:get_marshaller_config(grpc),
    ?assert(is_list(Config)).

get_marshaller_config_unsupported_test() ->
    ?assertException(throw,
                     {unsupported_format, invalid_format},
                     iso_8583_marshaller:get_marshaller_config(invalid_format)).

%%====================================================================
%% Get Supported Formats Tests
%%====================================================================

get_supported_formats_test() ->
    Formats = iso_8583_marshaller:get_supported_formats(),
    ?assert(lists:member(ascii, Formats)),
    ?assert(lists:member(binary, Formats)),
    ?assert(lists:member(ebcdic, Formats)),
    ?assert(lists:member(json, Formats)),
    ?assert(lists:member(xml, Formats)),
    ?assert(lists:member(grpc, Formats)).


%%====================================================================
%% Module Exports Tests
%%====================================================================

module_exports_test_() ->
    [?_assert(erlang:function_exported(iso_8583_marshaller, marshal, 2)),
     ?_assert(erlang:function_exported(iso_8583_marshaller, marshal, 3)),
     ?_assert(erlang:function_exported(iso_8583_marshaller, unmarshal, 2)),
     ?_assert(erlang:function_exported(iso_8583_marshaller, unmarshal, 3)),
     ?_assert(erlang:function_exported(iso_8583_marshaller, get_marshaller_config, 1)),
     ?_assert(erlang:function_exported(iso_8583_marshaller, get_supported_formats, 0))].

%%====================================================================
%% XML Unmarshal Tests (unmarshal works correctly)
%%====================================================================

xml_unmarshal_simple_message_test() ->
    XmlStr = "<isomsg><field id=\"0\" value=\"0200\" /></isomsg>",
    Msg = iso_8583_marshaller:unmarshal(XmlStr, xml),
    ?assert(iso_8583:is_message(Msg)),
    Val = iso_8583:get(0, Msg),
    ?assert(Val =:= "0200" orelse Val =:= <<"0200">>).

xml_unmarshal_multiple_fields_test() ->
    XmlStr = "<isomsg><field id=\"0\" value=\"0200\" /><field id=\"2\" value=\"4111111111111111\" /></isomsg>",
    Msg = iso_8583_marshaller:unmarshal(XmlStr, xml),
    ?assert(iso_8583:is_message(Msg)),
    ?assert(iso_8583:has_field(0, Msg)),
    ?assert(iso_8583:has_field(2, Msg)).

xml_unmarshal_with_encoding_rules_test() ->
    XmlStr = "<isomsg><field id=\"0\" value=\"0200\" /></isomsg>",
    Msg = iso_8583_marshaller:unmarshal(XmlStr, xml, iso_8583_fields_1987),
    ?assert(iso_8583:is_message(Msg)).

xml_unmarshal_with_undefined_encoding_rules_test() ->
    XmlStr = "<isomsg><field id=\"0\" value=\"0200\" /></isomsg>",
    Msg = iso_8583_marshaller:unmarshal(XmlStr, xml, undefined),
    ?assert(iso_8583:is_message(Msg)).

xml_unmarshal_with_1993_rules_test() ->
    XmlStr = "<isomsg><field id=\"0\" value=\"1200\" /></isomsg>",
    Msg = iso_8583_marshaller:unmarshal(XmlStr, xml, iso_8583_fields_1993),
    ?assert(iso_8583:is_message(Msg)).

xml_unmarshal_with_2003_rules_test() ->
    XmlStr = "<isomsg><field id=\"0\" value=\"2200\" /></isomsg>",
    Msg = iso_8583_marshaller:unmarshal(XmlStr, xml, iso_8583_fields_2003),
    ?assert(iso_8583:is_message(Msg)).

xml_unmarshal_unknown_version_test() ->
    XmlStr = "<isomsg><field id=\"0\" value=\"9200\" /></isomsg>",
    Msg = iso_8583_marshaller:unmarshal(XmlStr, xml),
    ?assert(iso_8583:is_message(Msg)).

xml_unmarshal_with_attributes_test() ->
    XmlStr = "<isomsg test_attr=\"test_val\"><field id=\"0\" value=\"0200\" /></isomsg>",
    Msg = iso_8583_marshaller:unmarshal(XmlStr, xml),
    ?assert(iso_8583:is_message(Msg)),
    ?assertEqual("test_val", iso_8583:get_attribute("test_attr", Msg)).
