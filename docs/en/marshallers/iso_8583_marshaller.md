# iso_8583_marshaller

## Description

Facade module for marshalling/unmarshalling ISO 8583 messages in different formats. Provides a unified and simplified interface to serialize and deserialize ISO 8583 messages.

## Supported Formats

- **ASCII** - ASCII hexadecimal string representation
- **Binary** - Compact binary format
- **EBCDIC** - For IBM mainframe systems
- **JSON** - JSON representation for modern APIs
- **XML** - XML representation for enterprise integrations
- **gRPC** - For gRPC communication

## Exported Functions

### Simplified API (Recommended)

#### `marshal/2`

Serializes an ISO 8583 message using a specific format. Uses default encoding rules based on the MTI version.

**Parameters:**
- `Message` - ISO 8583 message
- `Format` - Marshalling format (atom)

**Return:** `binary()` - Serialized data

**Valid formats:** `ascii | binary | ebcdic | json | xml | grpc`

**Example:**
```erlang
Msg = iso_8583:new(),
Msg1 = iso_8583:set_mti(<<"0200">>, Msg),
Msg2 = iso_8583:set(2, <<"4111111111111111">>, Msg1),

% Marshal to ASCII
AsciiData = iso_8583_marshaller:marshal(Msg2, ascii),

% Marshal to Binary
BinaryData = iso_8583_marshaller:marshal(Msg2, binary),

% Marshal to JSON
JsonData = iso_8583_marshaller:marshal(Msg2, json).
```

---

#### `marshal/3`

Serializes a message using specific format and encoding rules.

**Parameters:**
- `Message` - ISO 8583 message
- `Format` - Marshalling format
- `EncodingRules` - Encoding rules module or `undefined`

**Return:** `binary()`

**Example:**
```erlang
% Force use of ISO 8583:1987 rules
Data = iso_8583_marshaller:marshal(Msg, binary, iso_8583_fields_1987).
```

---

#### `unmarshal/2`

Deserializes data using a specific format. Automatically detects encoding rules based on the MTI.

**Parameters:**
- `Marshalled` - Serialized data (binary or list(byte()))
- `Format` - Format used in serialization

**Return:** `iso8583message()`

**Example:**
```erlang
% Unmarshal from ASCII
Msg = iso_8583_marshaller:unmarshal(AsciiData, ascii).

% Unmarshal from Binary
Msg = iso_8583_marshaller:unmarshal(BinaryData, binary).

% Unmarshal from JSON
Msg = iso_8583_marshaller:unmarshal(JsonData, json).
```

---

#### `unmarshal/3`

Deserializes data using specific format and encoding rules.

**Parameters:**
- `Marshalled` - Serialized data
- `Format` - Format
- `EncodingRules` - Rules module or `undefined`

**Return:** `iso8583message()`

**Example:**
```erlang
Msg = iso_8583_marshaller:unmarshal(Data, binary, iso_8583_fields_2003).
```

---

### Configuration Functions

#### `get_marshaller_config/1`

Returns the handler configuration for a specific format.

**Parameters:**
- `Format` - Marshalling format

**Return:** `list(marshal_handler())`

**Example:**
```erlang
Config = iso_8583_marshaller:get_marshaller_config(ascii).
% Returns list of handlers for ASCII format
```

---

#### `get_supported_formats/0`

Returns list of all supported marshalling formats.

**Return:** `list(marshal_format())`

**Example:**
```erlang
Formats = iso_8583_marshaller:get_supported_formats().
% Returns: [ascii, binary, ebcdic, json, xml, grpc]
```

---

### Low-Level API (Advanced)

#### `marshal_with_handlers/2`

Serializes a message using custom handlers.

**Parameters:**
- `Message` - ISO 8583 message
- `MarshalHandlers` - List of handlers

**Return:** `binary()`

**Example:**
```erlang
% For advanced cases with custom handlers
Handlers = iso_8583_marshaller:get_marshaller_config(ascii),
CustomHandlers = [{encoding_rules, my_custom_module} | Handlers],
Data = iso_8583_marshaller:marshal_with_handlers(Msg, CustomHandlers).
```

---

#### `unmarshal_with_handlers/2`

Deserializes data using custom handlers.

**Parameters:**
- `Marshalled` - Serialized data
- `MarshalHandlers` - List of handlers

**Return:** `iso8583message()`

---

## Handler Types

### `marshal_handler()`

Tuple that specifies a handler module and its function:

- `{field_marshaller, module()}` - Marshal/unmarshal fields
- `{bitmap_marshaller, module()}` - Marshal/unmarshal bitmap
- `{mti_marshaller, module()}` - Marshal/unmarshal MTI
- `{init_marshaller, module()}` - Initialize marshalling
- `{end_marshaller, module()}` - Finalize marshalling
- `{encoding_rules, module()}` - Field encoding rules
- `{field_arranger, module()}` - Field ordering

## Automatic Version Detection

The marshaller automatically detects the ISO 8583 version based on the first digit of the MTI:

| MTI Digit | Version | Rules Used |
|-----------|---------|------------|
| 0 | ISO 8583:1987 | `iso_8583_fields_1987` |
| 1 | ISO 8583:1993 | `iso_8583_fields_1993` |
| 2 | ISO 8583:2003 | `iso_8583_fields_2003` |

**Example:**
```erlang
% MTI = 0200 -> Automatically uses 1987 rules
% MTI = 1200 -> Automatically uses 1993 rules
% MTI = 2200 -> Automatically uses 2003 rules
```

## Usage Examples

### Basic Example

```erlang
% Create message
Msg = iso_8583:new(),
Msg1 = iso_8583:set_mti(<<"0200">>, Msg),
Msg2 = iso_8583:set(2, <<"4111111111111111">>, Msg1),
Msg3 = iso_8583:set(3, <<"000000">>, Msg2),
Msg4 = iso_8583:set(4, <<"000000001000">>, Msg3),

% Marshal for sending
BinaryData = iso_8583_marshaller:marshal(Msg4, binary),

% Send over network...
% Receive response...

% Unmarshal response
Response = iso_8583_marshaller:unmarshal(ReceivedData, binary).
```

### Example with Different Formats

```erlang
% Receive in ASCII format
AsciiMsg = iso_8583_marshaller:unmarshal(AsciiData, ascii),

% Convert to JSON (for logging or API)
JsonData = iso_8583_marshaller:marshal(AsciiMsg, json),

% Or convert to Binary (for network)
BinaryData = iso_8583_marshaller:marshal(AsciiMsg, binary).
```

### Example with Specific Version

```erlang
% Force use of ISO 8583:2003
Msg2003 = iso_8583_marshaller:unmarshal(
    Data,
    binary,
    iso_8583_fields_2003
).

% Marshal with 1987 rules
Data1987 = iso_8583_marshaller:marshal(
    Msg,
    ascii,
    iso_8583_fields_1987
).
```

### Complete Integration Example

```erlang
process_transaction(Socket) ->
    % Receive data from network
    {ok, Data} = gen_tcp:recv(Socket, 0),

    % Unmarshal
    Request = iso_8583_marshaller:unmarshal(Data, binary),

    % Process
    Response = process(Request),

    % Marshal response
    ResponseData = iso_8583_marshaller:marshal(Response, binary),

    % Send
    gen_tcp:send(Socket, ResponseData).
```

## Specific Marshalling Modules

For direct use of a specific format, see:

- [iso_8583_marshaller_ascii](iso_8583_marshaller_ascii.md)
- [iso_8583_marshaller_binary](iso_8583_marshaller_binary.md)
- [iso_8583_marshaller_ebcdic](iso_8583_marshaller_ebcdic.md)
- [iso_8583_marshaller_json](iso_8583_marshaller_json.md)
- [iso_8583_marshaller_xml](iso_8583_marshaller_xml.md)

## See Also

- [iso_8583](../modules/iso_8583.md) - Main message module
- [iso_8583_fields](../fields/iso_8583_fields.md) - Field definitions by version
- [Format Guide](formats.md) - Details about each format
