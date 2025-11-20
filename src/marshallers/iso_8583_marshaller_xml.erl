-module(iso_8583_marshaller_xml).


%%
%% Include files
%%
%% @headerfile "../include/iso_8583_types.hrl"
-include_lib("xmerl/include/xmerl.hrl").
-include("iso_8583_types.hrl").
-include("iso_8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([marshal/1,
		 unmarshal/1,
		 marshal_field/3, 
		 unmarshal_field/3,
		 marshal_end/2,
		 unmarshal_end/2,
		 marshal_init/1, 
		 unmarshal_init/2,
		 marshal_bitmap/1, 
		 unmarshal_bitmap/1,
		 marshal_mti/1, 
		 unmarshal_mti/1]).

%%
%% API Functions
%%

%% @doc Constructs an XML representation of
%%      an iso8583message().
%%
%% @spec marshal(iso8583message()) -> string()
-spec(marshal(iso8583message()) -> string()).

marshal(Message) ->
	iso_8583_marshaller:marshal(Message, ?MARSHALLER_XML).

%% @doc Constructs an iso8583message() from an XML document 
%%      representation of the message.
%%
%% @spec unmarshal(string()) -> iso8583message()
-spec(unmarshal(string()) -> iso8583message()).

unmarshal(Marshalled) ->
	iso_8583_marshaller:unmarshal(Marshalled, ?MARSHALLER_XML).

%% @doc Marshals a field value into an XML &lt;field&gt; element. The
%%      encoding rules module argument is ignored.
%%
%% @spec marshal_field(integer(), iso8583field_value(), module()) -> string()
-spec(marshal_field(integer(), iso8583field_value(), module()) -> string()).

marshal_field(FieldId, FieldValue, _EncodingRules) when is_list(FieldValue)->
	Id = integer_to_list(FieldId),
	"<field id=\"" ++ Id ++ "\" value=\"" ++ FieldValue ++ "\" />\n";
marshal_field(FieldId, FieldValue, _EncodingRules) when is_binary(FieldValue) ->
	Id = integer_to_list(FieldId),
	"<field id=\"" ++
		Id ++
		"\" value=\"" ++
		iso_8583_converters:binary_to_ascii_hex(FieldValue) ++
		"\" type=\"binary\" />\n";
% if we drop through to here, Value is of type iso8583message().
marshal_field(FieldId, FieldValue, _EncodingRules) ->
	true = iso_8583:is_message(FieldValue),
	Id = integer_to_list(FieldId),
	AttrKeys = iso_8583:get_attribute_keys(FieldValue),
	Attrs = [{Key, iso_8583:get_attribute(Key, FieldValue)} || Key <- AttrKeys],
	MarshalledContent = iso_8583_marshaller:marshal(FieldValue, [{field_marshaller, ?MODULE}, {mti_marshaller, ?MODULE}]),
	"<isomsg id=\"" ++
		Id ++
		"\"" ++
		encode_attributes(Attrs) ++
		">\n" ++
		binary_to_list(MarshalledContent) ++
		"</isomsg>\n".

%% @doc Extracts a field value from an XML document.  The field value,
%%      the document and an empty list is returned as a 3-tuple.
%%      A module that specifies how the field is encoded must be passed
%%      as an argument.
%%
%% @spec unmarshal_field(integer(), string(), module()) -> {iso8583field_value(), string(), list(integer())}
-spec(unmarshal_field(integer(), string(), module()) -> {iso8583field_value(), string(), list(integer())}).

unmarshal_field(FieldId, Marshalled, _EncodingRule) ->
	{Xml, []} = xmerl_scan:string(Marshalled),
	ChildNodes = Xml#xmlElement.content,
	FieldValue = unmarshal_field(FieldId, ChildNodes),
	{FieldValue, Marshalled, []}.

%% @doc Wraps XML elements in an &lt;isomsg&gt; XML element and
%%      returns the resultant XML document. The attributes of
%%      the original message are attributes of the &lt;isomsg&gt; 
%%      XML element.
%%
%% @spec marshal_end(iso8583message(), string()) -> string()
-spec(marshal_end(iso8583message(), string()) -> string()).

marshal_end(Message, Marshalled) ->
	AttrKeys = iso_8583:get_attribute_keys(Message),
	Attrs = [{Key, iso_8583:get_attribute(Key, Message)} || Key <- AttrKeys],
	"<isomsg" ++
		encode_attributes(Attrs) ++
		">\n" ++
		Marshalled ++
		"</isomsg>\n".
	
%% @doc Finishes the unmarshalling of a message and returns the
%%      message.
%%
%% @spec unmarshal_end(iso8583message(), string()) -> iso8583message()
-spec(unmarshal_end(iso8583message(), string()) -> iso8583message()).

unmarshal_end(Message, _Marshalled) ->
	Message.

%% @doc Starts the marshalling of a message and returns the
%%      initial marshalled data and message as a 2-tuple.
%%
%% @spec marshal_init(iso8583message()) -> {string(), iso8583message()}
-spec(marshal_init(iso8583message()) -> {string(), iso8583message()}).

marshal_init(Message) ->
	{[], Message}.

%% @doc Creates an ISO 8583 message by extracting the attributes of an &lt;isomsg&gt; XML 
%%      element and returns the message and the XML document as 
%%      a 2-tuple. No fields of the message are populated by this function; only
%%      the attributes of the message are set.
%%
%% @spec unmarshal_init(string(), string()) -> {iso8583message(), string()}
-spec(unmarshal_init(string(), string()) -> {iso8583message(), string()}).

unmarshal_init(Message, Marshalled) ->
	{Xml, []} = xmerl_scan:string(Marshalled),
	isomsg = Xml#xmlElement.name,
	Attrs = Xml#xmlElement.attributes,
	Msg = set_attributes(lists:reverse(attributes_to_list(Attrs, [])), Message),
	{Msg, Marshalled}.

%% @doc Returns an empty string and the message as a 2-tuple.
%%
%% @spec marshal_bitmap(list(integer())) -> string()
-spec(marshal_bitmap(list(integer())) -> string()).

marshal_bitmap(Message) ->
	{[], Message}.

%% @doc Extracts a list of field IDs from an XML 
%%      representation of an ISO 8583 message. The result is returned
%%      as a 2-tuple of the field IDs and the XML document.
%%
%% @spec unmarshal_bitmap(string()) -> {list(integer()), string()}
-spec(unmarshal_bitmap(string()) -> {list(integer()), string()}).

unmarshal_bitmap(Marshalled) ->
	{Xml, []} = xmerl_scan:string(Marshalled),
	ChildNodes = Xml#xmlElement.content,
	FieldIds = extract_ids(ChildNodes, []) -- [0],
	{lists:sort(FieldIds), Marshalled}.

%% @doc Marshals the MTI into an XML &lt;field&gt; element.
%%
%% @spec marshal_mti(string()) -> string()
-spec(marshal_mti(string()) -> string()).

marshal_mti(Mti) ->
	marshal_field(0, Mti, iso_8583_fields).

%% @doc Extracts the MTI from an XML
%%      document.  The MTI and the XML document are 
%%      returned as a 2-tuple.
%%
%% @spec unmarshal_mti(string()) -> {string(), string()}
-spec(unmarshal_mti(string()) -> {string(), string()}).

unmarshal_mti(Marshalled) ->
	{Mti, Rest, []} = unmarshal_field(0, Marshalled, iso_8583_fields),
	{Mti, Rest}.

%%
%% Local Functions
%%
encode_attributes(List) ->
	encode_attributes(List, "").

encode_attributes([], Result) ->
	Result;
encode_attributes([{Key, Value} | Tail], Result) ->
	encode_attributes(Tail, " " ++ Key ++ "=\"" ++ Value ++ "\"" ++  Result).

attributes_to_list([], Result) ->
	Result;
attributes_to_list([H|T], Result) ->
	Id = atom_to_list(H#xmlAttribute.name),
	Value = H#xmlAttribute.value,
	attributes_to_list(T, [{Id, Value} | Result]).

set_attributes([], Message) ->
	Message;
set_attributes([{Key, Value} | Tail], Message) ->
	set_attributes(Tail, iso_8583:set_attribute(Key, Value, Message)).

get_attribute_value(Key, [{Key, Value} | _Tail]) ->
	Value;
get_attribute_value(Key, [_Head|Tail]) ->
	get_attribute_value(Key, Tail).

extract_ids([], Result) ->
	Result;
extract_ids([Field|Tail], Result) when is_record(Field, xmlElement) ->
	Attributes = Field#xmlElement.attributes,
	AttributesList = attributes_to_list(Attributes, []),
	Id = get_attribute_value("id", AttributesList),
	FieldId = list_to_integer(Id),
	extract_ids(Tail, [FieldId|Result]);
extract_ids([_Field|Tail], Result) ->
	extract_ids(Tail, Result).

%% @doc Finds and unmarshals a specific field from a list of XML elements.
-spec unmarshal_field(integer(), list()) -> iso8583field_value().

unmarshal_field(TargetId, [Field|Tail]) when is_record(Field, xmlElement) ->
	Attributes = Field#xmlElement.attributes,
	AttributesList = attributes_to_list(Attributes, []),
	Id = get_attribute_value("id", AttributesList),
	FieldId = list_to_integer(Id),
	case FieldId of
		TargetId ->
			unmarshal_field(Field);
		_ ->
			unmarshal_field(TargetId, Tail)
	end;
unmarshal_field(TargetId, [_Field|Tail]) ->
	unmarshal_field(TargetId, Tail).

%% @doc Unmarshals a single XML element to a field value.
-spec unmarshal_field(tuple()) -> iso8583field_value().

unmarshal_field(FieldElement) ->
	Attributes = FieldElement#xmlElement.attributes,
	AttributesList = attributes_to_list(Attributes, []),
	Id = get_attribute_value("id", AttributesList),
	case FieldElement#xmlElement.name of
		field ->
			ValueStr = get_attribute_value("value", AttributesList),
			case is_attribute("type", AttributesList) of
				false ->
					ValueStr;
				true ->
					"binary" = get_attribute_value("type", AttributesList),
					iso_8583_converters:ascii_hex_to_binary(ValueStr)
			end;
		isomsg ->
			AttrsExceptId = AttributesList -- [{"id", Id}],
			ChildNodes = FieldElement#xmlElement.content,
			Message1 = iso_8583:new(),
			Message2 = set_attributes(lists:reverse(AttrsExceptId), Message1),
			unmarshal_complex(ChildNodes, Message2)
	end.

unmarshal_complex([], Iso8583Msg) ->
	Iso8583Msg;
unmarshal_complex([Field|T], Iso8583Msg) when is_record(Field, xmlElement) ->
	Attributes = Field#xmlElement.attributes,
	AttributesList = attributes_to_list(Attributes, []),
	Id = get_attribute_value("id", AttributesList),
	FieldId = list_to_integer(Id),
	Value = unmarshal_field(Field),
	UpdatedMsg = iso_8583:set(FieldId, Value, Iso8583Msg),
	unmarshal_complex(T, UpdatedMsg);
unmarshal_complex([_H|T], Iso8583Msg) ->
	unmarshal_complex(T, Iso8583Msg).

is_attribute(_Id, []) ->
	false;
is_attribute(Id, [{Id, _}|_Tail]) ->
	true;
is_attribute(Id, [_Head|Tail]) ->
	is_attribute(Id, Tail).