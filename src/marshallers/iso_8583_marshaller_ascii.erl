-module(iso_8583_marshaller_ascii).

%%
%% Include files
%%
%% @headerfile "../include/iso_8583_types.hrl"
-include("iso_8583_types.hrl").
-include("iso_8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([marshal/1, unmarshal/1, marshal_bitmap/1, unmarshal_bitmap/1, marshal_field/3,
         unmarshal_field/3, marshal_mti/1, unmarshal_mti/1, marshal_end/2, unmarshal_end/2]).

%%
%% API Functions
%%

%% @doc Constructs an ASCII string representation of
%%      an iso8583message().
%%
%% @spec marshal(iso8583message()) -> string()
-spec marshal(iso8583message()) -> string().
marshal(Message) ->
    iso_8583_marshaller:marshal(Message, ?MARSHALLER_ASCII).

%% @doc Constructs an iso8583message() from an ASCII string
%%      marshalling of the message.
%%
%% @spec unmarshal(string()) -> iso8583message()
-spec unmarshal(string()) -> iso8583message().
unmarshal(Marshalled) ->
    iso_8583_marshaller:unmarshal(Marshalled, ?MARSHALLER_ASCII).

%% @doc Constructs an ASCII string representation of the
%%      bitmap for an iso8583message().
%%
%% @spec marshal_bitmap(list(integer())) -> string()
-spec marshal_bitmap([integer()]) -> string().
marshal_bitmap(Message) ->
    FieldIds = iso_8583:get_fields(Message) -- [0],
    {Fields, UpdatedMessage} =
        case lists:max(FieldIds) > 64 of
            true ->
                F = [1] ++ FieldIds,
                SecondaryBitmap = iso_8583_converters:list_to_bitmap(F, 64),
                UM = iso_8583:set(1, SecondaryBitmap, Message),
                {F, UM};
            false ->
                {FieldIds, Message}
        end,
    {iso_8583_converters:binary_to_ascii_hex(
         iso_8583_converters:list_to_bitmap(Fields, 0)),
     UpdatedMessage}.

%% @doc Extracts a list of field IDs from an ASCII string
%%      representation of an ISO 8583 message. The result is returned
%%      as a 2-tuple of the field IDs and the remainder of the
%%      the message (encoding the field values but not the bit map).
%%
%% @spec unmarshal_bitmap(string()) -> {list(integer()), string()}
-spec unmarshal_bitmap(string()) -> {[integer()], string()}.
unmarshal_bitmap(AsciiMessage) ->
    {AsciiBitmap, Fields} = erlang:split_binary(AsciiMessage, 16),
    Bitmap =
        iso_8583_converters:ascii_hex_to_binary(
            erlang:binary_to_list(AsciiBitmap)),
    {iso_8583_converters:bitmap_to_list(Bitmap, 0), Fields}.

%% @doc Marshals a field value into an ASCII string using a specified
%%      encoding rules module.
%%
%% @spec marshal_field(integer(), iso8583field_value(), module()) -> string()
-spec marshal_field(integer(), iso8583field_value(), module()) -> string().
marshal_field(FieldId, FieldValue, EncodingRules) ->
    Pattern = EncodingRules:get_encoding(FieldId),
    marshal_data_element(Pattern, FieldValue).

%% @doc Extracts a field value from the start of a string.  The field value,
%%      the rest of the unmarshalled string and a list of additional field IDs
%%      that need to be unmarshalled is returned as a 3-tuple.
%%      A module that specifies how the field is encoded must be passed
%%      as an argument.
%%
%% @spec unmarshal_field(integer(), string(), module()) -> {iso8583field_value(), string(), list(integer())}
-spec unmarshal_field(integer(), string(), module()) ->
                         {iso8583field_value(), string(), [integer()]}.
unmarshal_field(1, AsciiFields, _EncodingRules) ->
    {Value, Rest} = unmarshal_data_element({b, fixed, 64}, AsciiFields),
    {Value, Rest, iso_8583_converters:bitmap_to_list(Value, 64)};
unmarshal_field(FieldId, AsciiFields, EncodingRules) ->
    Pattern = EncodingRules:get_encoding(FieldId),
    {FieldValue, MarshalledRest} = unmarshal_data_element(Pattern, AsciiFields),
    {FieldValue, MarshalledRest, []}.

%% @doc Marshals the MTI into an ASCII string.
%%
%% @spec marshal_mti(string()) -> string()
-spec marshal_mti(string()) -> string().
marshal_mti(Mti) ->
    marshal_field(0, Mti, iso_8583_fields).

%% @doc Extracts the MTI from the start of a string.  The MTI
%%      and the rest of the unmarshalled string is returned as a 2-tuple.
%%
%% @spec unmarshal_mti(string()) -> {string(), string()}
-spec unmarshal_mti(string()) -> {string(), string()}.
unmarshal_mti(Marshalled) ->
    {Mti, Rest, []} = unmarshal_field(0, Marshalled, iso_8583_fields),
    {Mti, Rest}.

%% @doc Completes the marshalling of a message and returns the marshalled
%%      form.
%%
%% @spec marshal_end(iso8583message(), string()) -> string()
-spec marshal_end(iso8583message(), string()) -> string().
marshal_end(_Message, Marshalled) ->
    Marshalled.

%% @doc Completes the unmarshalling of a message and returns the
%%      message.
%%
%% @spec unmarshal_end(iso8583message(), Marshalled::string()) -> iso8583message()
-spec unmarshal_end(iso8583message(), Marshalled :: string()) -> iso8583message().
unmarshal_end(Message, <<>>) ->
    iso_8583:remove(1, Message).

%%
%% Local Functions
%%
marshal_data_element({n, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
    iso_8583_converters:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({n, lllvar, Length}, FieldValue) when length(FieldValue) =< Length ->
    iso_8583_converters:integer_to_string(length(FieldValue), 3) ++ FieldValue;
marshal_data_element({ns, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
    iso_8583_converters:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({an, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
    iso_8583_converters:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({an, lllvar, Length}, FieldValue)
    when length(FieldValue) =< Length ->
    iso_8583_converters:integer_to_string(length(FieldValue), 3) ++ FieldValue;
marshal_data_element({ans, llvar, Length}, FieldValue)
    when length(FieldValue) =< Length ->
    iso_8583_converters:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({ans, lllvar, Length}, FieldValue)
    when length(FieldValue) =< Length ->
    iso_8583_converters:integer_to_string(length(FieldValue), 3) ++ FieldValue;
marshal_data_element({n, fixed, Length}, FieldValue) when length(FieldValue) =< Length ->
    IntValue = list_to_integer(FieldValue),
    iso_8583_converters:integer_to_string(IntValue, Length);
marshal_data_element({an, fixed, Length}, FieldValue) when length(FieldValue) =< Length ->
    iso_8583_converters:pad_with_trailing_spaces(FieldValue, Length);
marshal_data_element({ans, fixed, Length}, FieldValue)
    when length(FieldValue) =< Length ->
    iso_8583_converters:pad_with_trailing_spaces(FieldValue, Length);
marshal_data_element({x_n, fixed, Length}, [Head | FieldValue])
    when Head =:= $C orelse Head =:= $D ->
    IntValue = list_to_integer(FieldValue),
    [Head] ++ iso_8583_converters:integer_to_string(IntValue, Length);
marshal_data_element({z, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
    iso_8583_converters:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({b, fixed, Length}, FieldValue)
    when size(FieldValue) =:= Length div 8 ->
    iso_8583_converters:binary_to_ascii_hex(FieldValue).

unmarshal_data_element({n, llvar, _MaxLength}, AsciiFields) ->
    {N, Rest} = erlang:split_binary(AsciiFields, 2),
    erlang:split_binary(Rest, list_to_integer(binary_to_list(N)));
unmarshal_data_element({n, lllvar, _MaxLength}, AsciiFields) ->
    {N, Rest} = lists:split(3, AsciiFields),
    lists:split(list_to_integer(N), Rest);
unmarshal_data_element({ns, llvar, _MaxLength}, AsciiFields) ->
    {N, Rest} = lists:split(2, AsciiFields),
    lists:split(list_to_integer(N), Rest);
unmarshal_data_element({an, llvar, _MaxLength}, AsciiFields) ->
    {N, Rest} = lists:split(2, AsciiFields),
    lists:split(list_to_integer(N), Rest);
unmarshal_data_element({an, lllvar, _MaxLength}, AsciiFields) ->
    {N, Rest} = lists:split(3, AsciiFields),
    lists:split(list_to_integer(N), Rest);
unmarshal_data_element({ans, llvar, _MaxLength}, AsciiFields) ->
    {N, Rest} = lists:split(2, AsciiFields),
    lists:split(list_to_integer(N), Rest);
unmarshal_data_element({ans, lllvar, _MaxLength}, AsciiFields) ->
    {N, Rest} = lists:split(3, AsciiFields),
    lists:split(list_to_integer(N), Rest);
unmarshal_data_element({n, fixed, Length}, AsciiFields) ->
    erlang:split_binary(AsciiFields, Length);
unmarshal_data_element({an, fixed, Length}, AsciiFields) ->
    lists:split(Length, AsciiFields);
unmarshal_data_element({ans, fixed, Length}, AsciiFields) ->
    lists:split(Length, AsciiFields);
unmarshal_data_element({x_n, fixed, Length}, [Head | Tail])
    when Head =:= $C orelse Head =:= $D ->
    lists:split(Length + 1, [Head | Tail]);
unmarshal_data_element({z, llvar, _MaxLength}, AsciiFields) ->
    {N, Rest} = lists:split(2, AsciiFields),
    lists:split(list_to_integer(N), Rest);
unmarshal_data_element({b, fixed, Length}, AsciiFields) ->
    {ValueStr, Rest} = lists:split(Length div 4, AsciiFields),
    Value = iso_8583_converters:ascii_hex_to_binary(ValueStr),
    {Value, Rest}.
