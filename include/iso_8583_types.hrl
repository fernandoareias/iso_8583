-type utf8() :: binary().
%% @type bcd() = list(byte()). A list of bytes where each byte is a BCD value.
-type bcd() :: [byte()].
-type max_length() :: integer().
%% @type field_encoding() = {n|b|an|ans|x_n|ns|z, fixed|llvar|lllvar, max_length()}.
%% How a field is encoded.
-type field_encoding() ::
    {n | b | an | ans | x_n | ns | z, fixed | llvar | lllvar, max_length()}.

%% An encapsulation of an ISO 8583 message.
%%
%% @type iso8583message() = #iso8583_message{attributes=[iso8583attribute()], values=dict()}. An
%% encapsulation of an ISO 8583 message.
-record(iso8583_message, {attributes = [], values = dict:new()}).

-type iso8583message() :: #iso8583_message{}.
%% Valid types for the field of an ISO 8583 message.
%%
%% @type iso8583field_value() = utf8()|binary()|iso8583message(). Valid
%% types for an ISO 8583 field.
-type iso8583field_value() :: utf8() | binary() | iso8583message().
