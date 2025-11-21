-module(iso_8583_message_utils).

%%
%% ISO 8583 Message Utility Functions
%%
%% This module provides utility functions for working with ISO 8583 messages,
%% including creating responses, handling repeats, cloning fields, and MTI manipulation.
%%

%%
%% Include files
%%
-include("iso_8583_types.hrl").
-include("iso_8583_fields_id.hrl").

%%
%% Exported Functions
%%
-export([create_response/1, create_response/2, create_response_with_code/2,
         create_response_with_code/3, mark_as_repeat/1, is_repeat/1, clone_fields/1,
         clone_fields/2, clone_fields/3, copy_fields/3, get_mti_type/1, get_mti_version/1,
         is_request/1, is_response/1, is_advice/1, is_reversal/1, has_required_fields/2,
         validate_mti/1]).

                              % Response creation

    % Repeat handling

    % Field operations

    % MTI operations

    % Message validation

%%
%% Response Creation Functions
%%

%% @doc Creates a response message with the same field values as the original message.
%%      Automatically changes MTI to indicate it's a response and clears repeat flag.
%%
%% @spec create_response(iso8583message()) -> iso8583message()
-spec create_response(iso8583message()) -> iso8583message().
create_response(Message) ->
    create_response(iso_8583:get_fields(Message), Message).

%% @doc Creates a response message copying only specified fields from the original.
%%      Useful when you want to return a subset of fields in the response.
%%
%% @spec create_response(list(integer()), iso8583message()) -> iso8583message()
-spec create_response([integer()], iso8583message()) -> iso8583message().
create_response(FieldIds, Message) ->
    Clone = clone_fields(FieldIds, Message),
    convert_to_response_mti(Clone).

%% @doc Creates a response message with a specific response code.
%%      The response code is set in field 39 (RESP_CODE).
%%
%% @spec create_response_with_code(iso8583message(), binary()) -> iso8583message()
-spec create_response_with_code(iso8583message(), binary()) -> iso8583message().
create_response_with_code(Message, ResponseCode) ->
    Response = create_response(Message),
    iso_8583:set(?RESP_CODE, ResponseCode, Response).

%% @doc Creates a response message with specific fields and response code.
%%
%% @spec create_response_with_code(list(integer()), iso8583message(), binary()) -> iso8583message()
-spec create_response_with_code([integer()], iso8583message(), binary()) ->
                                   iso8583message().
create_response_with_code(FieldIds, Message, ResponseCode) ->
    Response = create_response(FieldIds, Message),
    iso_8583:set(?RESP_CODE, ResponseCode, Response).

%%
%% Repeat Handling Functions
%%

%% @doc Marks a message as a repeat by updating the MTI.
%%      Changes the last digit of MTI according to ISO 8583 repeat rules.
%%
%% @spec mark_as_repeat(iso8583message()) -> iso8583message()
-spec mark_as_repeat(iso8583message()) -> iso8583message().
mark_as_repeat(Message) ->
    <<M1, M2, M3, M4>> = iso_8583:get(?MTI, Message),
    M4Updated =
        case M4 of
            $0 ->
                $1;  % Request -> Repeat request
            $1 ->
                $1;  % Already repeat request
            $2 ->
                $3;  % Response -> Repeat response
            $3 ->
                $3;  % Already repeat response
            $4 ->
                $5;  % Advice -> Repeat advice
            $5 ->
                $5;  % Already repeat advice
            _ ->
                M4
        end,
    iso_8583:set(?MTI, <<M1, M2, M3, M4Updated>>, Message).

%% @doc Checks if a message is marked as a repeat.
%%
%% @spec is_repeat(iso8583message()) -> boolean()
-spec is_repeat(iso8583message()) -> boolean().
is_repeat(Message) ->
    <<_M1, _M2, _M3, M4>> = iso_8583:get(?MTI, Message),
    M4 =:= $1 orelse M4 =:= $3 orelse M4 =:= $5.

%%
%% Field Operations
%%

%% @doc Clones all fields from source message to a new message.
%%
%% @spec clone_fields(iso8583message()) -> iso8583message()
-spec clone_fields(iso8583message()) -> iso8583message().
clone_fields(Message) ->
    clone_fields(iso_8583:get_fields(Message), Message).

%% @doc Clones specified fields from source message to a new message.
%%
%% @spec clone_fields(list(integer()), iso8583message()) -> iso8583message()
-spec clone_fields([integer()], iso8583message()) -> iso8583message().
clone_fields(FieldIds, Message) ->
    clone_fields(FieldIds, Message, iso_8583:new()).

%% @doc Clones specified fields from source to target message.
%%
%% @spec clone_fields(list(integer()), iso8583message(), iso8583message()) -> iso8583message()
-spec clone_fields([integer()], iso8583message(), iso8583message()) -> iso8583message().
clone_fields([], _Source, Target) ->
    Target;
clone_fields([FieldId | Tail], Source, Target) ->
    case iso_8583:has_field(FieldId, Source) of
        true ->
            Value = iso_8583:get(FieldId, Source),
            UpdatedTarget = iso_8583:set(FieldId, Value, Target),
            clone_fields(Tail, Source, UpdatedTarget);
        false ->
            clone_fields(Tail, Source, Target)
    end.

%% @doc Copies specified fields from source to target message (in-place).
%%      Unlike clone_fields, this modifies the existing target message.
%%
%% @spec copy_fields(list(integer()), iso8583message(), iso8583message()) -> iso8583message()
-spec copy_fields([integer()], iso8583message(), iso8583message()) -> iso8583message().
copy_fields(FieldIds, Source, Target) ->
    clone_fields(FieldIds, Source, Target).

%%
%% MTI Operations
%%

%% @doc Extracts the message type from MTI (position 3).
%%      Returns: request | response | advice | notification | unknown
%%
%% @spec get_mti_type(iso8583message()) -> request | response | advice | notification | unknown
-spec get_mti_type(iso8583message()) ->
                      request | response | advice | notification | unknown.
get_mti_type(Message) ->
    <<_M1, _M2, M3, _M4>> = iso_8583:get(?MTI, Message),
    case M3 of
        $0 ->
            request;
        $1 ->
            request;
        $2 ->
            response;
        $3 ->
            response;
        $4 ->
            advice;
        $5 ->
            advice;
        $6 ->
            notification;
        $7 ->
            notification;
        _ ->
            unknown
    end.

%% @doc Extracts the ISO 8583 version from MTI (position 1).
%%
%% @spec get_mti_version(iso8583message()) -> '1987' | '1993' | '2003' | unknown
-spec get_mti_version(iso8583message()) -> '1987' | '1993' | '2003' | unknown.
get_mti_version(Message) ->
    <<M1, _M2, _M3, _M4>> = iso_8583:get(?MTI, Message),
    case M1 of
        $0 ->
            '1987';
        $1 ->
            '1993';
        $2 ->
            '2003';
        _ ->
            unknown
    end.

%% @doc Checks if message is a request.
%%
%% @spec is_request(iso8583message()) -> boolean()
-spec is_request(iso8583message()) -> boolean().
is_request(Message) ->
    get_mti_type(Message) =:= request.

%% @doc Checks if message is a response.
%%
%% @spec is_response(iso8583message()) -> boolean()
-spec is_response(iso8583message()) -> boolean().
is_response(Message) ->
    get_mti_type(Message) =:= response.

%% @doc Checks if message is an advice.
%%
%% @spec is_advice(iso8583message()) -> boolean()
-spec is_advice(iso8583message()) -> boolean().
is_advice(Message) ->
    get_mti_type(Message) =:= advice.

%% @doc Checks if message is a reversal (based on processing code).
%%
%% @spec is_reversal(iso8583message()) -> boolean()
-spec is_reversal(iso8583message()) -> boolean().
is_reversal(Message) ->
    case iso_8583:has_field(?PROC_CODE, Message) of
        true ->
            ProcCode = iso_8583:get(?PROC_CODE, Message),
            % Check if transaction type (first 2 digits) indicates reversal
            <<T1, T2, _Rest/binary>> = ProcCode,
            T1 =:= $0 andalso T2 =:= $4;  % 04xxxx = reversal
        false ->
            false
    end.

%%
%% Message Validation
%%

%% @doc Checks if all required fields are present in the message.
%%
%% @spec has_required_fields(list(integer()), iso8583message()) -> boolean()
-spec has_required_fields([integer()], iso8583message()) -> boolean().
has_required_fields(RequiredFields, Message) ->
    lists:all(fun(FieldId) -> iso_8583:has_field(FieldId, Message) end, RequiredFields).

%% @doc Validates MTI format (must be 4 digits).
%%
%% @spec validate_mti(iso8583message()) -> ok | {error, invalid_mti}
-spec validate_mti(iso8583message()) -> ok | {error, invalid_mti}.
validate_mti(Message) ->
    case iso_8583:has_field(?MTI, Message) of
        true ->
            Mti = iso_8583:get(?MTI, Message),
            case byte_size(Mti) of
                4 ->
                    % Check if all are digits
                    <<M1, M2, M3, M4>> = Mti,
                    case is_digit(M1) andalso is_digit(M2) andalso is_digit(M3) andalso is_digit(M4)
                    of
                        true ->
                            ok;
                        false ->
                            {error, invalid_mti}
                    end;
                _ ->
                    {error, invalid_mti}
            end;
        false ->
            {error, invalid_mti}
    end.

%%
%% Local Functions
%%

convert_to_response_mti(Message) ->
    <<M1, M2, M3, M4>> = iso_8583:get(?MTI, Message),
    % Convert request/advice to response and clear repeat flag
    M3Response =
        case M3 of
            $0 ->
                $1;  % Request -> Response request
            $1 ->
                $1;  % Already response request
            $2 ->
                $3;  % Response -> Response response
            $3 ->
                $3;  % Already response response
            $4 ->
                $5;  % Advice -> Response advice
            $5 ->
                $5;  % Already response advice
            _ ->
                M3
        end,
    % Clear repeat flag (set to 0 or 2)
    M4Response = M4 div 2 * 2,
    iso_8583:set(?MTI, <<M1, M2, M3Response, M4Response>>, Message).

is_digit(C) when C >= $0 andalso C =< $9 ->
    true;
is_digit(_) ->
    false.
