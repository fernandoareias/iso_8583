# iso_8583_message_utils

## Description

Utility module for working with ISO 8583 messages. Provides helper functions for creating responses, handling repeats, cloning fields, analyzing MTI, and validating messages.

## Exported Functions

### Response Creation

#### `create_response/1`

Creates a response message with the same field values as the original message. Automatically changes the MTI to indicate it's a response and clears the repeat flag.

**Parameters:**
- `Message` - Original ISO 8583 message (request)

**Return:** `iso8583message()` - Response message

**Example:**
```erlang
Request = iso_8583_marshaller:unmarshal(Data, binary),
Response = iso_8583_message_utils:create_response(Request).
```

---

#### `create_response/2`

Creates a response message copying only the specified fields from the original message. Useful when you want to return only a subset of fields in the response.

**Parameters:**
- `FieldIds` - List of field IDs to copy
- `Message` - Original ISO 8583 message

**Return:** `iso8583message()`

**Example:**
```erlang
% Copy only essential fields for response
Response = iso_8583_message_utils:create_response(
    [2, 3, 4, 11, 41, 42],  % PAN, ProcCode, Amount, STAN, Terminal, Merchant
    Request
).
```

---

#### `create_response_with_code/2`

Creates a response message with a specific response code. The response code is set in field 39 (RESP_CODE).

**Parameters:**
- `Message` - Original ISO 8583 message
- `ResponseCode` - Response code (binary)

**Return:** `iso8583message()`

**Example:**
```erlang
% Create approved response
Response = iso_8583_message_utils:create_response_with_code(
    Request,
    <<"00">>  % Approved
).

% Create declined response
Response = iso_8583_message_utils:create_response_with_code(
    Request,
    <<"51">>  % Insufficient funds
).
```

---

#### `create_response_with_code/3`

Creates a response message with specific fields and response code.

**Parameters:**
- `FieldIds` - List of field IDs to copy
- `Message` - Original ISO 8583 message
- `ResponseCode` - Response code

**Return:** `iso8583message()`

**Example:**
```erlang
Response = iso_8583_message_utils:create_response_with_code(
    [2, 3, 4, 11, 41, 42],
    Request,
    <<"00">>
).
```

---

### Repeat Management

#### `mark_as_repeat/1`

Marks a message as a repeat by updating the MTI. Changes the last digit of the MTI according to ISO 8583 repeat rules.

**Conversion rules:**
- `0` → `1` (Request → Repeat request)
- `2` → `3` (Response → Repeat response)
- `4` → `5` (Advice → Repeat advice)

**Parameters:**
- `Message` - ISO 8583 message

**Return:** `iso8583message()`

**Example:**
```erlang
RepeatMsg = iso_8583_message_utils:mark_as_repeat(OriginalMsg).
```

---

#### `is_repeat/1`

Checks if a message is marked as a repeat.

**Parameters:**
- `Message` - ISO 8583 message

**Return:** `boolean()`

**Example:**
```erlang
case iso_8583_message_utils:is_repeat(Msg) of
    true -> process_as_repeat(Msg);
    false -> process_normally(Msg)
end.
```

---

### Field Operations

#### `clone_fields/1`

Clones all fields from a message to a new message.

**Parameters:**
- `Message` - Source message

**Return:** `iso8583message()`

**Example:**
```erlang
Clone = iso_8583_message_utils:clone_fields(Original).
```

---

#### `clone_fields/2`

Clones specific fields from a source message to a new message.

**Parameters:**
- `FieldIds` - List of field IDs to clone
- `Message` - Source message

**Return:** `iso8583message()`

**Example:**
```erlang
% Clone only identification fields
Clone = iso_8583_message_utils:clone_fields([2, 11, 41, 42], Original).
```

---

#### `clone_fields/3`

Clones specific fields from the source message to the target message.

**Parameters:**
- `FieldIds` - List of field IDs
- `Source` - Source message
- `Target` - Target message

**Return:** `iso8583message()` - Updated target message

**Example:**
```erlang
Updated = iso_8583_message_utils:clone_fields(
    [2, 3, 4],
    SourceMsg,
    TargetMsg
).
```

---

#### `copy_fields/3`

Copies specific fields from one message to another (in-place). Similar to `clone_fields/3`.

**Parameters:**
- `FieldIds` - List of IDs
- `Source` - Source message
- `Target` - Target message

**Return:** `iso8583message()`

---

### MTI Analysis

#### `get_mti_type/1`

Extracts the message type from the MTI (position 3).

**Parameters:**
- `Message` - ISO 8583 message

**Return:** `request | response | advice | notification | unknown`

**MTI examples:**
- `0200` → `request` (authorization request)
- `0210` → `response` (authorization response)
- `0420` → `advice` (reversal advice)
- `0810` → `notification` (network management)

**Example:**
```erlang
case iso_8583_message_utils:get_mti_type(Msg) of
    request -> process_request(Msg);
    response -> process_response(Msg);
    advice -> process_advice(Msg);
    _ -> {error, unknown_type}
end.
```

---

#### `get_mti_version/1`

Extracts the ISO 8583 version from the MTI (position 1).

**Parameters:**
- `Message` - ISO 8583 message

**Return:** `'1987' | '1993' | '2003' | unknown`

**MTI examples:**
- `0200` → `'1987'`
- `1200` → `'1993'`
- `2200` → `'2003'`

**Example:**
```erlang
Version = iso_8583_message_utils:get_mti_version(Msg),
EncodingRules = case Version of
    '1987' -> iso_8583_fields_1987;
    '1993' -> iso_8583_fields_1993;
    '2003' -> iso_8583_fields_2003;
    _ -> iso_8583_fields
end.
```

---

#### `is_request/1`

Checks if the message is a request.

**Parameters:**
- `Message` - ISO 8583 message

**Return:** `boolean()`

---

#### `is_response/1`

Checks if the message is a response.

**Parameters:**
- `Message` - ISO 8583 message

**Return:** `boolean()`

---

#### `is_advice/1`

Checks if the message is an advice.

**Parameters:**
- `Message` - ISO 8583 message

**Return:** `boolean()`

---

#### `is_reversal/1`

Checks if the message is a reversal (based on processing code).

**Parameters:**
- `Message` - ISO 8583 message

**Return:** `boolean()`

**Note:** Identifies reversals by checking if the processing code (field 3) starts with "04".

**Example:**
```erlang
case iso_8583_message_utils:is_reversal(Msg) of
    true -> process_reversal(Msg);
    false -> process_normal_transaction(Msg)
end.
```

---

### Message Validation

#### `has_required_fields/2`

Checks if all required fields are present in the message.

**Parameters:**
- `RequiredFields` - List of required field IDs
- `Message` - ISO 8583 message

**Return:** `boolean()`

**Example:**
```erlang
RequiredFields = [2, 3, 4, 11, 41, 42],
case iso_8583_message_utils:has_required_fields(RequiredFields, Msg) of
    true -> process(Msg);
    false -> {error, missing_required_fields}
end.
```

---

#### `validate_mti/1`

Validates the MTI format (must be 4 digits).

**Parameters:**
- `Message` - ISO 8583 message

**Return:** `ok | {error, invalid_mti}`

**Example:**
```erlang
case iso_8583_message_utils:validate_mti(Msg) of
    ok -> process(Msg);
    {error, invalid_mti} -> reject(Msg)
end.
```

---

## Complete Example

```erlang
%% Process request and create response
process_request(Data) ->
    % Unmarshal message
    Request = iso_8583_marshaller:unmarshal(Data, binary),

    % Validate MTI
    ok = iso_8583_message_utils:validate_mti(Request),

    % Check if it's a request
    true = iso_8583_message_utils:is_request(Request),

    % Check required fields
    RequiredFields = [2, 3, 4, 11, 41, 42],
    case iso_8583_message_utils:has_required_fields(RequiredFields, Request) of
        false ->
            {error, missing_fields};
        true ->
            % Process transaction
            ResultCode = process_transaction(Request),

            % Create response with code
            Response = iso_8583_message_utils:create_response_with_code(
                [2, 3, 4, 11, 41, 42],  % Fields to copy
                Request,
                ResultCode
            ),

            % Marshal and return
            {ok, iso_8583_marshaller:marshal(Response, binary)}
    end.

%% Handle repeat
handle_message(Data) ->
    Msg = iso_8583_marshaller:unmarshal(Data, binary),

    case iso_8583_message_utils:is_repeat(Msg) of
        true ->
            % Retrieve and resend original response
            retrieve_and_resend_original_response(Msg);
        false ->
            % Process normally
            process_new_transaction(Msg)
    end.
```

## See Also

- [iso_8583](../modules/iso_8583.md) - Main message module
- [iso_8583_fields](../fields/iso_8583_fields.md) - Field definitions
- [iso_8583_marshaller](../marshallers/iso_8583_marshaller.md) - Serialization
