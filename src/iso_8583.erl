-module(iso_8583).

-include("iso_8583_types.hrl").
-include("iso_8583_fields_id.hrl").

%%
%% Exported Functions
%%
-export([new/0,
		 set/3,
		 set_mti/2,
		 get/2,
		 get/3,
		 get_mti/1,
		 get_fields/1,
		 has_field/2,
		 remove/2,
		 set_attribute/3,
		 get_attribute/2,
		 get_attribute/3,
		 get_attribute_keys/1,
		 remove_attribute/2,
		 is_message/1
		]).

%%
%% API Functions
%%

%% @doc Returns an empty ISO 8583 message.
%%
%% @spec new() -> iso8583message()
-spec(new() -> iso8583message()).

new() ->
	#iso8583_message{}.

%% @doc Sets the value of a field in a message and returns an updated
%%      message. The field can be specified as an integer or as a
%%      list of integers.  A list of integers indicates that
%%      some field is a submessage; e.g. [127, 2] would indicate field 2
%%      in field 127 of the original message. The value must be a 
%%      string, a binary or a nested message.
%%
%% @spec set(FieldId::integer()|list(integer()), iso8583field_value(), iso8583message()) -> iso8583message()
-spec(set(FieldId::integer()|list(integer()), iso8583field_value(), iso8583message()) -> iso8583message()).

set([FieldId], FieldValue, Message) when is_integer(FieldId) ->
	set(FieldId, FieldValue, Message);
set([FieldId|Tail], FieldValue, Message) when is_integer(FieldId) ->
	Message2 = case lists:member(FieldId, get_fields(Message)) of
		true ->
			get(FieldId, Message);
		false ->
			new()
	end,
	Message3 = set(Tail, FieldValue, Message2),
	set(FieldId, Message3, Message);
set(FieldId, FieldValue, #iso8583_message{values=Dict}=Message) when is_integer(FieldId) andalso FieldId >= 0 ->
	ConvertedFieldValue = convert_field_value(FieldValue),
	Message#iso8583_message{values=dict:store(FieldId, ConvertedFieldValue, Dict)}.

%% @doc Gets the value of a field from a message given a field ID or a list
%%      of identifiers. A list of integers indicates that
%%      some field is a submessage; e.g. [127, 2] would indicate subfield 2
%%      in field 127 of the original message.
%%
%% @spec get(FieldId::integer()|list(integer()), iso8583message()) -> iso8583field_value()
-spec(get(FieldId::integer()|list(integer()), iso8583message()) -> iso8583field_value()).

get([FieldId], Message) when is_integer(FieldId) ->
	get(FieldId, Message);
get([FieldId|Tail], Message)  when is_integer(FieldId) ->
	Message2 = get(FieldId, Message),
	get(Tail, Message2);
get(FieldId, #iso8583_message{values=Dict}) when is_integer(FieldId) ->
	dict:fetch(FieldId, Dict).

%% @doc Gets the value of a field from a message with a default value.
%%
%% @spec get(FieldId::integer()|list(integer()), iso8583message(), iso8583field_value()) -> iso8583field_value()
-spec(get(FieldId::integer()|list(integer()), iso8583message(), iso8583field_value()) -> iso8583field_value()).

get(FieldId, #iso8583_message{values=Dict}, Default) when is_integer(FieldId) ->
	case dict:find(FieldId, Dict) of
		{ok, Value} -> Value;
		error -> Default
	end;
get([FieldId], Message, Default) when is_integer(FieldId) ->
	get(FieldId, Message, Default);
get([FieldId|Tail], Message, Default) when is_integer(FieldId) ->
	case dict:find(FieldId, Message#iso8583_message.values) of
		{ok, Message2} when is_record(Message2, iso8583_message) ->
			get(Tail, Message2, Default);
		_ ->
			Default
	end.

%% @doc Checks if a field exists in a message.
%%
%% @spec has_field(FieldId::integer()|list(integer()), iso8583message()) -> boolean()
-spec(has_field(FieldId::integer()|list(integer()), iso8583message()) -> boolean()).

has_field(FieldId, #iso8583_message{values=Dict}) when is_integer(FieldId) ->
	dict:is_key(FieldId, Dict);
has_field([FieldId], Message) when is_integer(FieldId) ->
	has_field(FieldId, Message);
has_field([FieldId|Tail], Message) when is_integer(FieldId) ->
	case dict:find(FieldId, Message#iso8583_message.values) of
		{ok, Message2} when is_record(Message2, iso8583_message) ->
			has_field(Tail, Message2);
		_ ->
			false
	end.

%% @doc Gets the field IDs from a message; i.e. what fields exist in
%%      a message.
%%
%% @spec get_fields(iso8583message()) -> list(integer())
-spec(get_fields(iso8583message()) -> list(integer())).

get_fields(#iso8583_message{values=Dict}) ->
	lists:sort(dict:fetch_keys(Dict)).

%% @doc Returns a list of attribute keys for a message.
%%
%% @spec get_attribute_keys(iso8583message()) -> list(any())
-spec(get_attribute_keys(iso8583message()) -> list(any())).

get_attribute_keys(#iso8583_message{attributes=Attrs}) ->
	[Key || {Key, _Value} <- Attrs].

%% @doc Gets the value of an attribute of a message.
%%
%% @spec get_attribute(any(), iso8583message()) -> any()
-spec(get_attribute(any(), iso8583message()) -> any()).

get_attribute(Key, #iso8583_message{attributes=Attrs}) ->
	case [Value || {KeyId, Value} <- Attrs, KeyId =:= Key] of
		[Result] -> Result;
		[] -> throw({attribute_not_found, Key})
	end.

%% @doc Gets the value of an attribute of a message with a default value.
%%
%% @spec get_attribute(any(), iso8583message(), any()) -> any()
-spec(get_attribute(any(), iso8583message(), any()) -> any()).

get_attribute(Key, #iso8583_message{attributes=Attrs}, Default) ->
	case [Value || {KeyId, Value} <- Attrs, KeyId =:= Key] of
		[Result] -> Result;
		[] -> Default
	end.

%% @doc Sets the value of an attribute of a message.
%%
%% @spec set_attribute(any(), any(), iso8583message()) -> iso8583message()
-spec(set_attribute(any(), any(), iso8583message()) -> iso8583message()).

set_attribute(Key, Value, Message) ->
	UpdatedMessage = remove_attribute(Key, Message),
	Attrs = UpdatedMessage#iso8583_message.attributes,
	UpdatedMessage#iso8583_message{attributes=Attrs ++ [{Key, Value}]}.
	
%% @doc Removes a field from a message and returns the updated message.
%%
%% @spec remove(FieldId::integer()|list(integer()), iso8583message()) -> iso8583message()
-spec(remove(FieldId::integer()|list(integer()), iso8583message()) -> iso8583message()).

remove([FieldId], Message) ->
	remove(FieldId, Message);
remove([FieldId|Tail], #iso8583_message{values=Dict}=Message) ->
	case dict:is_key(FieldId, Dict) of
		false ->
			Message;
		true ->
			UpdatedSubfield = remove(Tail, get(FieldId, Message)),
			case get_fields(UpdatedSubfield) of
				[] ->
					remove(FieldId, Message);
		_ 		->
					set(FieldId, UpdatedSubfield, Message)
			end
	end;
remove(FieldId, #iso8583_message{values=Dict}=Message) ->
	UpdatedDict = dict:erase(FieldId, Dict),
	Message#iso8583_message{values=UpdatedDict}.
	
%% @doc Removes an attribute of a message.
%%
%% @spec remove_attribute(any(), iso8583message()) -> iso8583message()
-spec(remove_attribute(any(), iso8583message()) -> iso8583message()).

remove_attribute(Key, #iso8583_message{attributes=Attrs} = Message) ->
	UpdatedAttrs = [{KeyId, Val} || {KeyId, Val} <- Attrs, KeyId =/= Key],
	Message#iso8583_message{attributes=UpdatedAttrs}.

%% @doc A convenient function for setting the message type identifier (MTI)
%%      of a message.
%%
%% @spec set_mti(string(), iso8583message()) -> iso8583message()
-spec(set_mti(string(), iso8583message()) -> iso8583message()).

set_mti(Mti, Message) ->
	set(0, Mti, Message).

%% @doc A convenient function for getting the message type identifier (MTI)
%%      of a message.
%%
%% @spec get_mti(iso8583message()) -> string()
-spec(get_mti(iso8583message()) -> string()).

get_mti(Message) ->
	get(0, Message).

%% @doc A function for checking whether the type of a message is an iso8583message().
%%
%% @spec is_message(any()) -> boolean()
-spec(is_message(any()) -> boolean()).

is_message(#iso8583_message{}) ->
	true;
is_message(_NonMessage) ->
	false.
	
%%
%% Local Functions
%%

%% Field values can be stored only as binaries or messages.
convert_field_value(Value) when is_binary(Value) ->
	Value;
convert_field_value(Value) when is_list(Value) ->
	unicode:characters_to_binary(Value, utf8);
convert_field_value(Value) ->
	true = is_message(Value),
	Value.