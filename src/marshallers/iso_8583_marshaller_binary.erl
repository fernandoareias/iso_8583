-module(iso_8583_marshaller_binary).




%%
%% Include files
%%
%% @headerfile "../include/iso_8583_types.hrl"
-include("iso_8583_types.hrl").
-include("iso_8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([marshal/1,
		 unmarshal/1,
		 marshal_bitmap/1, 
		 unmarshal_bitmap/1,
		 marshal_field/3, 
		 unmarshal_field/3,
		 marshal_mti/1, 
		 unmarshal_mti/1,
		 marshal_end/2,
		 unmarshal_end/2]).


%%
%% API Functions
%%

%% @doc Marshals an iso8583message() to a list of bytes.
%%
%% @spec marshal(iso8583message()) -> list(byte())
-spec(marshal(iso8583message()) -> list(byte())).

marshal(Message) ->
	iso_8583_marshaller:marshal(Message, ?MARSHALLER_BINARY).

%% @doc Unmarshals a list of bytes to an iso8583message().
%%
%% @spec unmarshal(list(byte())) -> iso8583message()
-spec(unmarshal(list(byte())) -> iso8583message()).

unmarshal(Marshalled) ->
	iso_8583_marshaller:unmarshal(Marshalled, ?MARSHALLER_BINARY).
	
%% @doc Constructs a list of bytes representation of the
%%      bitmap for an iso8583message().
%%
%% @spec marshal_bitmap(list(integer())) -> list(byte())
-spec(marshal_bitmap(list(integer())) -> list(byte())).

marshal_bitmap(Message) ->
	FieldIds = iso_8583:get_fields(Message) -- [0],
	{Fields, UpdatedMessage} = case lists:max(FieldIds) > 64 of
		true ->
			F = [1] ++ FieldIds,
			SecondaryBitmap = iso_8583_converters:list_to_bitmap(F, 64),
			UM = iso_8583:set(1, SecondaryBitmap, Message),
			{F, UM};
		false ->
			{FieldIds, Message}
	end,
	{binary_to_list(iso_8583_converters:list_to_bitmap(Fields, 0)), UpdatedMessage}.

%% @doc Extracts a list of field IDs from a list of bytes representation of 
%%      an ISO 8583 message.  The result is returned as a 2-tuple: a list
%%      of field IDs and the remainder of the message excluding the bit map.
%%
%% @spec unmarshal_bitmap(list(byte())) -> {list(integer()), list(byte())}
-spec(unmarshal_bitmap(list(byte())) -> {list(integer()), list(byte())}).

unmarshal_bitmap(BinaryMessage) ->
	{Bitmap, Fields} = lists:split(8, BinaryMessage),
	{iso_8583_converters:bitmap_to_list(list_to_binary(Bitmap), 0), Fields}.

%% @doc Marshals a field value into a byte list using a specified
%%      encoding rules module.
%%
%% @spec marshal_field(integer(), iso8583field_value(), module()) -> list(byte())
-spec(marshal_field(integer(), iso8583field_value(), module()) -> list(byte())).

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
-spec(unmarshal_field(integer(), string(), module()) -> {iso8583field_value(), string(), list(integer())}).

unmarshal_field(1, BinaryFields, _EncodingRules) ->
	{Value, Rest} = unmarshal_data_element({b, fixed, 64}, BinaryFields),
	{Value, Rest, iso_8583_converters:bitmap_to_list(Value, 64)};
unmarshal_field(FieldId, BinaryFields, EncodingRules) ->
	Pattern = EncodingRules:get_encoding(FieldId),
	case unmarshal_data_element(Pattern, BinaryFields) of
		{Value, Rest} ->
			{Value, Rest, []}
	end.

%% @doc Marshals the MTI into a byte list.
%%
%% @spec marshal_mti(string()) -> list(byte())
-spec(marshal_mti(string()) -> list(byte())).

marshal_mti(Mti) ->
	marshal_field(0, Mti, iso_8583_fields).

%% @doc Extracts the MTI from the start of a byte list.  The MTI 
%%      and the rest of the unmarshalled list is returned as a 2-tuple.
%%
%% @spec unmarshal_mti(list(byte())) -> {string(), list(byte())}
-spec(unmarshal_mti(list(byte())) -> {string(), list(byte())}).

unmarshal_mti(Marshalled) ->
	{Value, Rest, []} = unmarshal_field(0, Marshalled, iso_8583_fields),
	{Value, Rest}.

%% @doc Completes the marshalling of a message and returns the marshalled
%%      form.
%%
%% @spec marshal_end(iso8583message(), list(byte())) -> list(byte())
marshal_end(_Message, Marshalled) ->
	Marshalled.

%% @doc Completes the unmarshalling of a message and returns the
%%      message.
%%
%% @spec unmarshal_end(iso8583message(), Marshalled::list(byte())) -> iso8583message()
-spec(unmarshal_end(iso8583message(), Marshalled::list(byte())) -> iso8583message()).

unmarshal_end(Message, []) ->
	iso_8583:remove(1, Message).

%%
%% Local Functions
%%

%% Helper function to convert field value to list if it's a binary
field_value_to_list(FieldValue) when is_binary(FieldValue) ->
	binary_to_list(FieldValue);
field_value_to_list(FieldValue) when is_list(FieldValue) ->
	FieldValue.

marshal_data_element({n, llvar, Length}, FieldValue) ->
	Value = field_value_to_list(FieldValue),
	case length(Value) =< Length of
		true ->
			LField = iso_8583_converters:integer_to_bcd(length(Value), 2),
			VField = iso_8583_converters:ascii_hex_to_bcd(Value, "0"),
			LField ++ VField
	end;
marshal_data_element({z, llvar, Length}, FieldValue) ->
	Value = field_value_to_list(FieldValue),
	case length(Value) =< Length of
		true ->
			LField = iso_8583_converters:integer_to_bcd(length(Value), 2),
			VField = iso_8583_converters:string_to_track2(Value),
			LField ++ VField
	end;
marshal_data_element({n, fixed, Length}, FieldValue) ->
	Value = field_value_to_list(FieldValue),
	PaddedValue = case Length rem 2 of
		0 ->
			iso_8583_converters:integer_to_string(list_to_integer(Value), Length);
		1 ->
			iso_8583_converters:integer_to_string(list_to_integer(Value), Length+1)
	end,
	iso_8583_converters:ascii_hex_to_bcd(PaddedValue, "0");
marshal_data_element({an, fixed, Length}, FieldValue) ->
	Value = field_value_to_list(FieldValue),
	iso_8583_converters:pad_with_trailing_spaces(Value, Length);
marshal_data_element({ans, fixed, Length}, FieldValue) ->
	Value = field_value_to_list(FieldValue),
	iso_8583_converters:pad_with_trailing_spaces(Value, Length);
marshal_data_element({an, llvar, Length}, FieldValue) ->
	Value = field_value_to_list(FieldValue),
	case length(Value) =< Length of
		true ->
			LField = iso_8583_converters:integer_to_bcd(length(Value), 2),
			LField ++ Value
	end;
marshal_data_element({ns, llvar, Length}, FieldValue) ->
	Value = field_value_to_list(FieldValue),
	case length(Value) =< Length of
		true ->
			LField = iso_8583_converters:integer_to_bcd(length(Value), 2),
			LField ++ Value
	end;
marshal_data_element({ans, llvar, Length}, FieldValue) ->
	Value = field_value_to_list(FieldValue),
	case length(Value) =< Length of
		true ->
			LField = iso_8583_converters:integer_to_bcd(length(Value), 2),
			LField ++ Value
	end;
marshal_data_element({ans, lllvar, Length}, FieldValue) ->
	Value = field_value_to_list(FieldValue),
	case length(Value) =< Length of
		true ->
			LField = iso_8583_converters:integer_to_bcd(length(Value), 3),
			LField ++ Value
	end;
marshal_data_element({x_n, fixed, Length}, FieldValue) ->
	Value = field_value_to_list(FieldValue),
	[Head | Rest] = Value,
	case Head =:= $C orelse Head =:= $D of
		true ->
			IntValue = list_to_integer(Rest),
			[Head|iso_8583_converters:integer_to_bcd(IntValue, Length)]
	end;
marshal_data_element({b, fixed, Length}, FieldValue) when size(FieldValue) =:= Length div 8->
	binary_to_list(FieldValue).

unmarshal_data_element({n, llvar, _MaxLength}, BinaryFields) ->
	[NBin|RestBin] = BinaryFields,
	N = iso_8583_converters:bcd_to_integer([NBin]),
	{ValueBin, Rest} = lists:split((N+1) div 2, RestBin), 
	{iso_8583_converters:bcd_to_ascii_hex(ValueBin, N, "0"), Rest};
unmarshal_data_element({n, lllvar, _MaxLength}, BinaryFields) ->
	{NBin, RestBin} = lists:split(2, BinaryFields),
	N = iso_8583_converters:bcd_to_integer(NBin),
	{ValueBin, Rest} = lists:split((N+1) div 2, RestBin), 
	{iso_8583_converters:bcd_to_ascii_hex(ValueBin, N, "0"), Rest};
unmarshal_data_element({an, llvar, _MaxLength}, BinaryFields) ->
	[NBin|RestBin] = BinaryFields,
	N = iso_8583_converters:bcd_to_integer([NBin]),
	lists:split(N, RestBin); 
unmarshal_data_element({ns, llvar, _MaxLength}, BinaryFields) ->
	[NBin|Rest] = BinaryFields,
	N = iso_8583_converters:bcd_to_integer([NBin]),
	lists:split(N, Rest); 
unmarshal_data_element({ans, llvar, _MaxLength}, BinaryFields) ->
	[NBin|Rest] = BinaryFields,
	N = iso_8583_converters:bcd_to_integer([NBin]),
	lists:split(N, Rest); 
unmarshal_data_element({ans, lllvar, _MaxLength}, BinaryFields) ->
	{NBin, Rest} = lists:split(2, BinaryFields),
	N = iso_8583_converters:bcd_to_integer(NBin),
	lists:split(N, Rest); 
unmarshal_data_element({n, fixed, Length}, BinaryFields) ->
	{NBin, RestBin} = lists:split((Length + 1) div 2, BinaryFields),
	case Length rem 2 of
		0 ->
			{iso_8583_converters:bcd_to_ascii_hex(NBin, Length, "0"), RestBin};
		1 ->
			[$0|AsciiHex] = iso_8583_converters:bcd_to_ascii_hex(NBin, Length+1, "0"),
			{AsciiHex, RestBin}
	end;
unmarshal_data_element({an, fixed, Length}, BinaryFields) ->
	{Field, Rest} = lists:split(Length, BinaryFields),
	{iso_8583_converters:pad_with_trailing_spaces(Field, Length), Rest};
unmarshal_data_element({ans, fixed, Length}, BinaryFields) ->
	{Field, Rest} = lists:split(Length, BinaryFields),
	{iso_8583_converters:pad_with_trailing_spaces(Field, Length), Rest};
unmarshal_data_element({x_n, fixed, Length}, BinaryFields) ->
	{Field, Rest} = lists:split(Length div 2 + 1, BinaryFields),
	{[X], Value} = lists:split(1, Field),
	ValueStr = iso_8583_converters:bcd_to_ascii_hex(Value, Length, "0"),
	case X =:= $C orelse X =:= $D of
		true ->
			{[X] ++ ValueStr, Rest}
	end;
unmarshal_data_element({z, llvar, _MaxLength}, BinaryFields) ->
	[NBin|RestBin] = BinaryFields,
	N = iso_8583_converters:bcd_to_integer([NBin]),
	{ValueBin, Rest} = lists:split((N+1) div 2, RestBin), 
	{iso_8583_converters:track2_to_string(ValueBin, N), Rest};
unmarshal_data_element({b, fixed, Length}, BinaryFields) ->
	{Bin, Rest} = lists:split(Length div 8, BinaryFields),
	{list_to_binary(Bin), Rest}.