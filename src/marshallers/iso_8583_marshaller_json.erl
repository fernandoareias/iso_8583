-module(iso_8583_marshaller_json).

%%
%% Include files
%%
%% @headerfile "../include/iso_8583_types.hrl"
-include("iso_8583_types.hrl").
-include("iso_8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([marshal/1, unmarshal/1, marshal_init/1, unmarshal_init/2, marshal_mti/1,
         unmarshal_mti/1, marshal_bitmap/1, unmarshal_bitmap/1, marshal_field/3, unmarshal_field/3,
         marshal_end/2, unmarshal_end/2]).

%%
%% API Functions
%%

%% @doc Constructs a JSON document of
%%      an iso8583message().
%%
%% @spec marshal(iso8583message()) -> string()
-spec marshal(iso8583message()) -> string().
marshal(Message) ->
    iso_8583_marshaller:marshal(Message, ?MARSHALLER_JSON).

%% @doc Constructs an iso8583message() from a JSON document
%%      representation of the message.
%%
%% @spec unmarshal(string()) -> iso8583message()
-spec unmarshal(string()) -> iso8583message().
unmarshal(Marshalled) ->
    iso_8583_marshaller:unmarshal(Marshalled, ?MARSHALLER_JSON).

%% @doc Creates an ISO 8583 message by extracting the attributes in a JSON
%%      document and returns the message and the JSON document as
%%      a 2-tuple. No fields of the message are populated by this function; only
%%      the attributes of the message are set.
%%
%% @spec unmarshal_init(string(), string()) -> {iso8583message(), string()}
-spec unmarshal_init(string(), string()) -> {iso8583message(), string()}.
unmarshal_init(Message, Marshalled) ->
    {struct, JsonData} = mochijson2:decode(Marshalled),
    Keys = proplists:get_keys(JsonData) -- [<<"iso8583_fields">>],
    Attrs =
        [{binary_to_list(Key), binary_to_list(proplists:get_value(Key, JsonData))}
         || Key <- Keys],
    {set_attributes(lists:reverse(Attrs), Message), Marshalled}.

%% @doc Starts the marshalling of a message and returns the
%%      initial marshalled data and message as a 2-tuple.
%%
%% @spec marshal_init(iso8583message()) -> {string(), iso8583message()}
-spec marshal_init(iso8583message()) -> {string(), iso8583message()}.
marshal_init(Message) ->
    {[], Message}.

%% @doc Extracts the MTI from a JSON document.
%%      The MTI and the JSON document are
%%      returned as a 2-tuple.
%%
%% @spec unmarshal_mti(string()) -> {string(), string()}
-spec unmarshal_mti(string()) -> {string(), string()}.
unmarshal_mti(Marshalled) ->
    {struct, JsonData} = mochijson2:decode(Marshalled),
    {struct, FieldsData} = proplists:get_value(<<"iso8583_fields">>, JsonData),
    MtiBin = proplists:get_value(<<"0">>, FieldsData),
    {binary_to_list(MtiBin), Marshalled}.

%% @doc Marshals the MTI into a JSON field.
%%
%% @spec marshal_mti(string()) -> string()
-spec marshal_mti(string()) -> string().
marshal_mti(Mti) ->
    encode_id(0) ++ " : " ++ encode_value(Mti).

%% @doc Extracts a list of field IDs from a JSON
%%      representation of an ISO 8583 message. The result is returned
%%      as a 2-tuple of the field IDs and the JSON document.
%%
%% @spec unmarshal_bitmap(string()) -> {list(integer()), string()}
-spec unmarshal_bitmap(string()) -> {[integer()], string()}.
unmarshal_bitmap(Marshalled) ->
    {struct, JsonData} = mochijson2:decode(Marshalled),
    {struct, FieldsData} = proplists:get_value(<<"iso8583_fields">>, JsonData),
    Fields = proplists:get_keys(FieldsData),
    FieldIds = [list_to_integer(binary_to_list(Id)) || Id <- Fields] -- [0],
    {lists:sort(FieldIds), Marshalled}.

%% @doc Returns an empty string and the message as a 2-tuple.
%%
%% @spec marshal_bitmap(list(integer())) -> string()
-spec marshal_bitmap([integer()]) -> string().
marshal_bitmap(Message) ->
    {[], Message}.

%% @doc Extracts a field value from a JSON document.  The field value,
%%      the document and an empty list is returned as a 3-tuple.
%%      A module that specifies how the field is encoded must be passed
%%      as an argument.
%%
%% @spec unmarshal_field(integer(), string(), module()) -> {iso8583field_value(), string(), list(integer())}
-spec unmarshal_field(integer(), string(), module()) ->
                         {iso8583field_value(), string(), [integer()]}.
unmarshal_field(FieldId, Marshalled, EncodingRules) ->
    {struct, JsonData} = mochijson2:decode(Marshalled),
    {struct, FieldsProps} = proplists:get_value(<<"iso8583_fields">>, JsonData),
    FieldValue = proplists:get_value(list_to_binary(integer_to_list(FieldId)), FieldsProps),
    Value =
        case FieldValue of
            {struct, PropList} ->
                unmarshal_complex_field([FieldId], iso_8583:new(), PropList, EncodingRules);
            _ ->
                unmarshal_simple_field(FieldId, FieldValue, EncodingRules)
        end,
    {Value, Marshalled, []}.

%% @doc Marshals a field value into a JSON field. The
%%      encoding rules module argument is ignored.
%%
%% @spec marshal_field(integer(), iso8583field_value(), module()) -> string()
-spec marshal_field(integer(), iso8583field_value(), module()) -> string().
marshal_field(FieldId, FieldValue, _EncodingRules) when is_list(FieldValue) ->
    ", " ++ encode_id(FieldId) ++ " : " ++ encode_value(FieldValue);
marshal_field(FieldId, FieldValue, _EncodingRules) when is_binary(FieldValue) ->
    ", " ++ encode_id(FieldId) ++ " : " ++ encode_value(FieldValue);
marshal_field(FieldId, FieldValue, _EncodingRules) ->
    true = iso_8583:is_message(FieldValue),
    Keys = iso_8583:get_fields(FieldValue),
    KeyValueList = [{Key, iso_8583:get(Key, FieldValue)} || Key <- Keys],
    ", " ++ encode_id(FieldId) ++ " : " ++ "{" ++ encode_values(KeyValueList, []) ++ "}".

%% @doc Finishes the unmarshalling of a message and returns the
%%      message.
%%
%% @spec unmarshal_end(iso8583message(), string()) -> iso8583message()
-spec unmarshal_end(iso8583message(), string()) -> iso8583message().
unmarshal_end(Message, _Marshalled) ->
    Message.

%% @doc Wraps the marshalled fields into an enclosing element
%%      and returns the marshalled result.
%%
%% @spec marshal_end(iso8583message(), string()) -> string()
-spec marshal_end(iso8583message(), string()) -> string().
marshal_end(Message, Marshalled) ->
    "{\"iso8583_fields\" : {" ++ Marshalled ++ "}" ++ marshal_attributes(Message) ++ "}".

%%
%% Local Functions
%%
unmarshal_simple_field(FieldId, FieldValue, EncodingRules) ->
    case EncodingRules:get_encoding(FieldId) of
        {b, _, _} ->
            iso_8583_converters:ascii_hex_to_binary(binary_to_list(FieldValue));
        _ ->
            binary_to_list(FieldValue)
    end.

unmarshal_complex_field(FieldId, Message, PropList, EncodingRules) ->
    ConstructMessageFun =
        fun(Id, MessageAccum) ->
           IdInt = list_to_integer(binary_to_list(Id)),
           UpdatedId = FieldId ++ [IdInt],
           FieldValue = proplists:get_value(Id, PropList),
           Value =
               case FieldValue of
                   {struct, PropList2} ->
                       unmarshal_complex_field(UpdatedId, iso_8583:new(), PropList2, EncodingRules);
                   _ ->
                       unmarshal_simple_field(UpdatedId, FieldValue, EncodingRules)
               end,
           iso_8583:set(IdInt, Value, MessageAccum)
        end,
    SubFieldIds = proplists:get_keys(PropList),
    lists:foldl(ConstructMessageFun, Message, SubFieldIds).

encode_values([], Result) ->
    Result;
encode_values([{Id, Value} | Tail], []) ->
    encode_values(Tail, encode_id(Id) ++ " : " ++ encode_value(Value));
encode_values([{Id, Value} | Tail], Result) ->
    encode_values(Tail, Result ++ ", " ++ encode_id(Id) ++ " : " ++ encode_value(Value)).

encode_id(Id) ->
    "\"" ++ integer_to_list(Id) ++ "\"".

encode_value(Value) when is_list(Value) ->
    "\"" ++ Value ++ "\"";
encode_value(Value) when is_binary(Value) ->
    "\"" ++ iso_8583_converters:binary_to_ascii_hex(Value) ++ "\"";
encode_value(Value) ->
    true = iso_8583:is_message(Value),
    Keys = iso_8583:get_fields(Value),
    KeyValueList = [{Key, iso_8583:get(Key, Value)} || Key <- Keys],
    "{" ++ encode_values(KeyValueList, []) ++ "}".

marshal_attributes(Message) ->
    AttrKeys = iso_8583:get_attribute_keys(Message),
    case AttrKeys of
        [] ->
            [];
        _ ->
            Attrs = [{Key, iso_8583:get_attribute(Key, Message)} || Key <- AttrKeys],
            ", " ++ marshal_attributes_list(Attrs)
    end.

marshal_attributes_list([{Key, Value}]) ->
    "\"" ++ Key ++ "\" : \"" ++ Value ++ "\"";
marshal_attributes_list([{Key, Value} | Tail]) ->
    "\"" ++ Key ++ "\" : \"" ++ Value ++ "\", " ++ marshal_attributes_list(Tail).

set_attributes([], Message) ->
    Message;
set_attributes([{Key, Value} | Tail], Message) ->
    set_attributes(Tail, iso_8583:set_attribute(Key, Value, Message)).
