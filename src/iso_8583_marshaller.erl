-module(iso_8583_marshaller).

%%
%% ISO 8583 Marshaller Facade Module
%%
%% This module provides a unified interface for marshalling/unmarshalling
%% ISO 8583 messages in different formats (ASCII, Binary, EBCDIC, JSON, XML).
%%
%% It supports both the low-level handler-based API and a simplified format-based API.
%%

%%
%% Include files
%%
-include("iso_8583_types.hrl").
-include("iso_8583_marshallers.hrl").

%%
%% Records
%%
-record(marshal_options, {
    field_marshaller,
    bitmap_marshaller,
    mti_marshaller,
    init_marshaller,
    end_marshaller,
    field_arranger,
    encoding_rules
}).

%%
%% Type definitions
%%
-type marshal_format() :: ascii | binary | ebcdic | json | xml | grpc.

-type marshal_handler() ::
    {bitmap_marshaller, module()} |
    {field_marshaller, module()} |
    {mti_marshaller, module()} |
    {init_marshaller, module()} |
    {end_marshaller, module()} |
    {field_arranger, module()} |
    {encoding_rules, module()}.

%%
%% Exported Functions
%%
-export([
    % Simplified API (recommended)
    marshal/2,
    unmarshal/2,
    marshal/3,
    unmarshal/3,

    % Get marshaller configuration
    get_marshaller_config/1,
    get_supported_formats/0
]).

%%
%% Simplified API Functions
%%

%% @doc Marshals an ISO 8583 message using a specified format.
%%      Uses default encoding rules based on MTI version.
%%
%% @spec marshal(iso8583message(), marshal_format()) -> binary()
-spec(marshal(iso8583message(), marshal_format()) -> binary()).

marshal(Message, Format) when is_atom(Format) ->
    MarshalHandlers = get_marshaller_config(Format),
    marshal_with_handlers(Message, MarshalHandlers).

%% @doc Marshals an ISO 8583 message using a specified format and encoding rules.
%%
%% @spec marshal(iso8583message(), marshal_format(), module() | undefined) -> binary()
-spec(marshal(iso8583message(), marshal_format(), module() | undefined) -> binary()).

marshal(Message, Format, EncodingRules) when is_atom(Format) ->
    MarshalHandlers = get_marshaller_config(Format),
    Handlers = case EncodingRules of
        undefined -> MarshalHandlers;
        _ -> [{encoding_rules, EncodingRules} | MarshalHandlers]
    end,
    marshal_with_handlers(Message, Handlers).

%% @doc Unmarshals a byte sequence using a specified format.
%%      Uses default encoding rules based on MTI version.
%%
%% @spec unmarshal(binary() | list(byte()), marshal_format()) -> iso8583message()
-spec(unmarshal(binary() | list(byte()), marshal_format()) -> iso8583message()).

unmarshal(Marshalled, Format) when is_atom(Format) ->
    MarshalHandlers = get_marshaller_config(Format),
    unmarshal_with_handlers(Marshalled, MarshalHandlers).

%% @doc Unmarshals a byte sequence using a specified format and encoding rules.
%%
%% @spec unmarshal(binary() | list(byte()), marshal_format(), module() | undefined) -> iso8583message()
-spec(unmarshal(binary() | list(byte()), marshal_format(), module() | undefined) -> iso8583message()).

unmarshal(Marshalled, Format, EncodingRules) when is_atom(Format) ->
    MarshalHandlers = get_marshaller_config(Format),
    Handlers = case EncodingRules of
        undefined -> MarshalHandlers;
        _ -> [{encoding_rules, EncodingRules} | MarshalHandlers]
    end,
    unmarshal_with_handlers(Marshalled, Handlers).

%%
%% Configuration Functions
%%

%% @doc Returns the marshaller configuration for a given format.
%%
%% @spec get_marshaller_config(marshal_format()) -> list(marshal_handler())
-spec(get_marshaller_config(marshal_format()) -> list(marshal_handler())).

get_marshaller_config(ascii) -> ?MARSHALLER_ASCII;
get_marshaller_config(binary) -> ?MARSHALLER_BINARY;
get_marshaller_config(ebcdic) -> ?MARSHALLER_EBCDIC;
get_marshaller_config(json) -> ?MARSHALLER_JSON;
get_marshaller_config(xml) -> ?MARSHALLER_XML;
get_marshaller_config(grpc) -> ?MARSHALLER_GRPC;
get_marshaller_config(Format) ->
    throw({unsupported_format, Format}).

%% @doc Returns a list of all supported marshalling formats.
%%
%% @spec get_supported_formats() -> list(marshal_format())
-spec(get_supported_formats() -> list(marshal_format())).

get_supported_formats() ->
    [ascii, binary, ebcdic, json, xml, grpc].

%%
%% Low-level Marshalling Functions (Handler-based API)
%%

%% @doc Marshals an ISO 8583 message into a byte sequence using handlers.
%%
%% @spec marshal_with_handlers(iso8583message(), list(marshal_handler())) -> binary()
-spec(marshal_with_handlers(iso8583message(), list(marshal_handler())) -> binary()).

marshal_with_handlers(Message, MarshalHandlers) ->
    OptionsRecord = parse_options(MarshalHandlers, #marshal_options{}),
    {Marshalled1, Message1} = init_marshalling(OptionsRecord, Message),
    MarshalledMti = encode_mti(OptionsRecord, Message1),
    Marshalled2 = concat_binary(Marshalled1, MarshalledMti),
    {MarshalledBitmap, Message2} = encode_bitmap(OptionsRecord, Message1),
    Marshalled3 = concat_binary(Marshalled2, MarshalledBitmap),
    MarshalledFields = encode_fields(OptionsRecord, Message2),
    Marshalled4 = concat_binary(Marshalled3, MarshalledFields),
    end_marshalling(OptionsRecord, Message2, Marshalled4).

%% @doc Unmarshals a byte sequence into an ISO 8583 message using handlers.
%%
%% @spec unmarshal_with_handlers(binary() | list(byte()), list(marshal_handler())) -> iso8583message()
-spec(unmarshal_with_handlers(binary() | list(byte()), list(marshal_handler())) -> iso8583message()).

unmarshal_with_handlers(Marshalled, MarshalHandlers) ->
    OptionsRecord = parse_options(MarshalHandlers, #marshal_options{}),
    {Message0, Marshalled1} = init_unmarshalling(OptionsRecord, iso_8583:new(), Marshalled),
    {Message1, Marshalled2} = decode_mti(OptionsRecord, Marshalled1, Message0),
    {FieldIds, Marshalled3} = decode_bitmap(OptionsRecord, Marshalled2),
    {Message2, Marshalled4} = decode_fields(FieldIds, Message1, OptionsRecord, Marshalled3),
    end_unmarshalling(OptionsRecord, Message2, Marshalled4).

%%
%% Local Functions
%%

%% Helper function to concatenate binaries or lists
concat_binary(A, B) when is_binary(A), is_binary(B) ->
    <<A/binary, B/binary>>;
concat_binary(A, B) when is_binary(A), is_list(B) ->
    <<A/binary, (list_to_binary(B))/binary>>;
concat_binary(A, B) when is_list(A), is_binary(B) ->
    <<(list_to_binary(A))/binary, B/binary>>;
concat_binary(A, B) when is_list(A), is_list(B) ->
    list_to_binary([A, B]).

parse_options([], OptionsRecord) ->
    OptionsRecord;
parse_options([{field_marshaller, Marshaller} | Tail], OptionsRecord) ->
    parse_options(Tail, OptionsRecord#marshal_options{field_marshaller = Marshaller});
parse_options([{bitmap_marshaller, Marshaller} | Tail], OptionsRecord) ->
    parse_options(Tail, OptionsRecord#marshal_options{bitmap_marshaller = Marshaller});
parse_options([{mti_marshaller, Marshaller} | Tail], OptionsRecord) ->
    parse_options(Tail, OptionsRecord#marshal_options{mti_marshaller = Marshaller});
parse_options([{init_marshaller, Marshaller} | Tail], OptionsRecord) ->
    parse_options(Tail, OptionsRecord#marshal_options{init_marshaller = Marshaller});
parse_options([{end_marshaller, Marshaller} | Tail], OptionsRecord) ->
    parse_options(Tail, OptionsRecord#marshal_options{end_marshaller = Marshaller});
parse_options([{encoding_rules, Rules} | Tail], OptionsRecord) ->
    parse_options(Tail, OptionsRecord#marshal_options{encoding_rules = Rules});
parse_options([{field_arranger, Arranger} | Tail], OptionsRecord) ->
    parse_options(Tail, OptionsRecord#marshal_options{field_arranger = Arranger}).

get_encoding_rules(Options, Message) ->
    if
        Options#marshal_options.encoding_rules =/= undefined ->
            Options#marshal_options.encoding_rules;
        Options#marshal_options.encoding_rules =:= undefined ->
            case iso_8583:get_fields(Message) of
                [0 | _Fields] ->
                    Mti = iso_8583:get(0, Message),
                    <<Version, _MtiRest/binary>> = Mti,
                    case Version of
                        $0 -> iso_8583_fields_1987;
                        $1 -> iso_8583_fields_1993;
                        $2 -> iso_8583_fields_2003;
                        _ -> iso_8583_fields
                    end;
                _ ->
                    iso_8583_fields
            end
    end.

encode_mti(Options, Message) ->
    MtiMarshalModule = Options#marshal_options.mti_marshaller,
    if
        MtiMarshalModule =:= undefined ->
            <<>>;
        MtiMarshalModule =/= undefined ->
            case iso_8583:get_fields(Message) of
                [0 | _Fields] ->
                    MtiMarshalModule:marshal_mti(iso_8583:get(0, Message));
                _ ->
                    <<>>
            end
    end.

decode_mti(Options, Marshalled, Message) ->
    MtiMarshalModule = Options#marshal_options.mti_marshaller,
    if
        MtiMarshalModule =:= undefined ->
            {Message, Marshalled};
        MtiMarshalModule =/= undefined ->
            {FieldValue, Rest} = MtiMarshalModule:unmarshal_mti(Marshalled),
            {iso_8583:set(0, FieldValue, Message), Rest}
    end.

encode_bitmap(Options, Message) ->
    BitmapMarshalModule = Options#marshal_options.bitmap_marshaller,
    if
        BitmapMarshalModule =:= undefined ->
            {<<>>, Message};
        BitmapMarshalModule =/= undefined ->
            BitmapMarshalModule:marshal_bitmap(Message)
    end.

decode_bitmap(Options, Marshalled) ->
    BitmapMarshalModule = Options#marshal_options.bitmap_marshaller,
    if
        BitmapMarshalModule =:= undefined ->
            {[], Marshalled};
        BitmapMarshalModule =/= undefined ->
            BitmapMarshalModule:unmarshal_bitmap(Marshalled)
    end.

encode_fields(Options, Message) ->
    Fields = iso_8583:get_fields(Message) -- [0],
    EncodingRules = get_encoding_rules(Options, Message),
    FieldMarshalModule = Options#marshal_options.field_marshaller,
    FieldArranger = Options#marshal_options.field_arranger,
    if
        FieldMarshalModule =:= undefined ->
            <<>>;
        FieldMarshalModule =/= undefined ->
            encode(Fields, Message, FieldMarshalModule, EncodingRules, FieldArranger)
    end.

encode(Fields, Msg, FieldMarshaller, EncodingRules, FieldArranger) ->
    encode(Fields, Msg, <<>>, FieldMarshaller, EncodingRules, FieldArranger).

encode([], _Msg, Result, _FieldMarshaller, _EncodingRules, _FieldArranger) ->
    Result;
encode(Fields, Msg, Result, FieldMarshaller, EncodingRules, FieldArranger) ->
    {FieldId, Tail} = if
        FieldArranger =:= undefined ->
            [FId | T] = Fields,
            {FId, T};
        true ->
            [FId | T] = FieldArranger:arrange_fields(Fields),
            {FId, T}
    end,
    Value = iso_8583:get(FieldId, Msg),
    EncodedValue = FieldMarshaller:marshal_field(FieldId, Value, EncodingRules),
    encode(Tail, Msg, concat_binary(Result, EncodedValue), FieldMarshaller, EncodingRules, undefined).

decode_fields([], Message, _OptionsRecord, Marshalled) ->
    {Message, Marshalled};
decode_fields(Fields, Message, Options, Marshalled) ->
    FieldArranger = Options#marshal_options.field_arranger,
    {FieldId, Tail} = if
        FieldArranger =:= undefined ->
            [FId | T] = Fields,
            {FId, T};
        FieldArranger =/= undefined ->
            [FId | T] = FieldArranger:arrange_fields(Fields),
            {FId, T}
    end,
    EncodingRules = get_encoding_rules(Options, Message),
    FieldMarshalModule = Options#marshal_options.field_marshaller,
    if
        FieldMarshalModule =:= undefined ->
            Message;
        FieldMarshalModule =/= undefined ->
            {FieldValue, Rest, FieldsToUnmarshal} = FieldMarshalModule:unmarshal_field(FieldId, Marshalled, EncodingRules),
            decode_fields(
                Tail ++ FieldsToUnmarshal,
                iso_8583:set(FieldId, FieldValue, Message),
                Options,
                Rest
            )
    end.

end_marshalling(Options, Message, Marshalled) ->
    EndMarshalModule = Options#marshal_options.end_marshaller,
    if
        EndMarshalModule =:= undefined ->
            Marshalled;
        EndMarshalModule =/= undefined ->
            EndMarshalModule:marshal_end(Message, Marshalled)
    end.

end_unmarshalling(Options, Message, Marshalled) ->
    EndMarshalModule = Options#marshal_options.end_marshaller,
    if
        EndMarshalModule =:= undefined ->
            Message;
        EndMarshalModule =/= undefined ->
            EndMarshalModule:unmarshal_end(Message, Marshalled)
    end.

init_marshalling(Options, Message) ->
    InitMarshalModule = Options#marshal_options.init_marshaller,
    if
        InitMarshalModule =:= undefined ->
            {<<>>, Message};
        InitMarshalModule =/= undefined ->
            InitMarshalModule:marshal_init(Message)
    end.

init_unmarshalling(Options, Marshalled, Message) ->
    WrapperMarshalModule = Options#marshal_options.init_marshaller,
    if
        WrapperMarshalModule =:= undefined ->
            {Marshalled, Message};
        WrapperMarshalModule =/= undefined ->
            WrapperMarshalModule:unmarshal_init(Marshalled, Message)
    end.
