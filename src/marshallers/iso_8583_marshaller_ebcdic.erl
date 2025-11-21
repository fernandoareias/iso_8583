-module(iso_8583_marshaller_ebcdic).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("iso_8583_types.hrl").
-include("iso_8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([marshal/1, unmarshal/1, marshal_field/3, unmarshal_field/3, marshal_mti/1,
         unmarshal_mti/1, marshal_bitmap/1, unmarshal_bitmap/1, marshal_end/2, unmarshal_end/2]).

%%
%% API Functions
%%

%% @doc Constructs an EBCDIC string representation of
%%      an iso8583message().
%%
%% @spec marshal(iso8583message()) -> list(byte())
-spec marshal(iso8583message()) -> [byte()].
marshal(Message) ->
    erl8583_marshaller:marshal(Message, ?MARSHALLER_EBCDIC).

%% @doc Constructs an iso8583message() from an EBCDIC string
%%      marshalling of the message.
%%
%% @spec unmarshal(list(byte())) -> iso8583message()
-spec unmarshal([byte()]) -> iso8583message().
unmarshal(Marshalled) ->
    erl8583_marshaller:unmarshal(Marshalled, ?MARSHALLER_EBCDIC).

%% @doc Marshals a field value into an EBCDIC string using a specified
%%      encoding rules module.
%%
%% @spec marshal_field(integer(), iso8583field_value(), module()) -> list(byte())
-spec marshal_field(integer(), iso8583field_value(), module()) -> [byte()].
marshal_field(FieldId, FieldValue, EncodingRules) ->
    Ascii = erl8583_marshaller_ascii:marshal_field(FieldId, FieldValue, EncodingRules),
    erl8583_convert:ascii_to_ebcdic(Ascii).

%% @doc Extracts a field value from the start of an EBCDIC string.  The field value
%%      the rest of the unmarshalled string and a list of additional field IDs
%%      that need to be unmarshalled is returned as a 3-tuple.
%%      A module that specifies how the field is encoded must be passed
%%      as an argument.
%%
%% @spec unmarshal_field(integer(), string(), module()) -> {iso8583field_value(), string(), list(integer())}
-spec unmarshal_field(integer(), string(), module()) ->
                         {iso8583field_value(), string(), [integer()]}.
unmarshal_field(FieldId, Marshalled, EncodingRules) ->
    Length = get_field_length(FieldId, Marshalled, EncodingRules),
    {Ebcdic, Rest} = lists:split(Length, Marshalled),
    Ascii = erl8583_convert:ebcdic_to_ascii(Ebcdic),
    {Field, [], Ids} =
        erl8583_marshaller_ascii:unmarshal_field(FieldId, Ascii, EncodingRules),
    {Field, Rest, Ids}.

%% @doc Marshals the MTI into an EBCDIC string.
%%
%% @spec marshal_mti(string()) -> list(byte())
-spec marshal_mti(string()) -> [byte()].
marshal_mti(Mti) ->
    marshal_field(0, Mti, erl8583_fields).

%% @doc Extracts the MTI from the start of an EBCDIC string.  The MTI
%%      and the rest of the unmarshalled string is returned as a 2-tuple.
%%
%% @spec unmarshal_mti(list(byte())) -> {string(), list(byte())}
-spec unmarshal_mti([byte()]) -> {string(), [byte()]}.
unmarshal_mti(Marshalled) ->
    {Mti, Rest, []} = unmarshal_field(0, Marshalled, erl8583_fields),
    {Mti, Rest}.

%% @doc Constructs an EBCDIC string representation of the
%%      bitmap for an iso8583message().
%%
%% @spec marshal_bitmap(list(integer())) -> list(byte())
-spec marshal_bitmap([integer()]) -> [byte()].
marshal_bitmap(Message) ->
    {AsciiBitmap, UpdatedMessage} = erl8583_marshaller_ascii:marshal_bitmap(Message),
    {erl8583_convert:ascii_to_ebcdic(AsciiBitmap), UpdatedMessage}.

%% @doc Extracts a list of field IDs from an EBCDIC string
%%      representation of an ISO 8583 message. The result is returned
%%      as a 2-tuple of the field IDs and the remainder of the
%%      the message (encoding the field values but not the bit map).
%%
%% @spec unmarshal_bitmap(list(byte())) -> {list(integer()), string()}
-spec unmarshal_bitmap([byte()]) -> {[integer()], string()}.
unmarshal_bitmap(Marshalled) ->
    Length = get_bitmap_length(Marshalled),
    {BitmapEbcdic, Rest} = lists:split(Length, Marshalled),
    {Bitmap, []} =
        erl8583_marshaller_ascii:unmarshal_bitmap(
            erl8583_convert:ebcdic_to_ascii(BitmapEbcdic)),
    {Bitmap, Rest}.

%% @doc Completes the marshalling of a message and returns the marshalled
%%      form.
%%
%% @spec marshal_end(iso8583message(), list(byte())) -> list(byte())
-spec marshal_end(iso8583message(), [byte()]) -> [byte()].
marshal_end(_Message, Marshalled) ->
    Marshalled.

%% @doc Completes the unmarshalling of a message and returns the
%%      message.
%%
%% @spec unmarshal_end(iso8583message(), Marshalled::list(byte())) -> iso8583message()
-spec unmarshal_end(iso8583message(), Marshalled :: [byte()]) -> iso8583message().
unmarshal_end(Message, []) ->
    erl8583_marshaller_ascii:unmarshal_end(Message, []).

%% Local Functions
%%
get_field_length(FieldId, Marshalled, EncodingRules) ->
    Encoding = EncodingRules:get_encoding(FieldId),
    case Encoding of
        {_, fixed, N} ->
            N;
        {_, llvar, _} ->
            Nebcdic = lists:sublist(Marshalled, 1, 2),
            Nascii = erl8583_convert:ebcdic_to_ascii(Nebcdic),
            list_to_integer(Nascii) + 2;
        {_, lllvar, _} ->
            Nebcdic = lists:sublist(Marshalled, 1, 3),
            Nascii = erl8583_convert:ebcdic_to_ascii(Nebcdic),
            list_to_integer(Nascii) + 3
    end.

get_bitmap_length(Msg) ->
    get_bitmap_length(Msg, 16).

get_bitmap_length(Msg, Length) ->
    [HexDig1, HexDig2 | _Tail] = Msg,
    EbcdicDigits = [HexDig1, HexDig2],
    <<Byte>> =
        erl8583_convert:ascii_hex_to_binary(
            erl8583_convert:ebcdic_to_ascii(EbcdicDigits)),
    case Byte band 128 of
        0 ->
            Length;
        _ ->
            {_Msg1, Msg2} = lists:split(16, Msg),
            get_bitmap_length(Msg2, Length + 16)
    end.
