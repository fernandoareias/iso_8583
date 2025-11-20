# ISO 8583 Documentation - Erlang

Welcome to the complete documentation of the ISO 8583 library in Erlang. This library provides a comprehensive set of tools for working with ISO 8583 messages, the international standard for financial transaction messages.

## 📚 Table of Contents

### Main Modules

- **[iso_8583](modules/iso_8583.md)** - Main module for creating and manipulating ISO 8583 messages
  - Message creation
  - Field manipulation
  - Attribute management
  - MTI (Message Type Indicator)

### Field Definitions

- **[iso_8583_fields](fields/iso_8583_fields.md)** - Facade for field definitions (all versions)
- **[iso_8583_fields_1987](fields/iso_8583_fields_1987.md)** - ISO 8583:1987 fields
- **[iso_8583_fields_1993](fields/iso_8583_fields_1993.md)** - ISO 8583:1993 fields
- **[iso_8583_fields_2003](fields/iso_8583_fields_2003.md)** - ISO 8583:2003 fields

### Marshalling (Serialization)

- **[iso_8583_marshaller](marshallers/iso_8583_marshaller.md)** - Main marshalling facade
  - Supports multiple formats: ASCII, Binary, EBCDIC, JSON, XML, gRPC
  - Automatic ISO version detection
  - Simplified and advanced APIs

#### Specific Marshallers

- **[iso_8583_marshaller_ascii](marshallers/iso_8583_marshaller_ascii.md)** - ASCII hexadecimal format
- **[iso_8583_marshaller_binary](marshallers/iso_8583_marshaller_binary.md)** - Compact binary format
- **[iso_8583_marshaller_ebcdic](marshallers/iso_8583_marshaller_ebcdic.md)** - EBCDIC format (mainframes)
- **[iso_8583_marshaller_json](marshallers/iso_8583_marshaller_json.md)** - JSON format
- **[iso_8583_marshaller_xml](marshallers/iso_8583_marshaller_xml.md)** - XML format

### Converters

- **[iso_8583_converters](converters/iso_8583_converters.md)** - Converters facade
- **[iso_8583_hex_converter](converters/iso_8583_hex_converter.md)** - Hexadecimal conversions
- **[iso_8583_numeric_converter](converters/iso_8583_numeric_converter.md)** - Numeric and BCD conversions
- **[iso_8583_ebcdic_converter](converters/iso_8583_ebcdic_converter.md)** - EBCDIC/ASCII conversions
- **[iso_8583_bitmap_converter](converters/iso_8583_bitmap_converter.md)** - Bitmap conversions
- **[iso_8583_track2_converter](converters/iso_8583_track2_converter.md)** - Track 2 conversions

### Utilities

- **[iso_8583_message_utils](utils/iso_8583_message_utils.md)** - Message utilities
  - Response creation
  - Repeat handling
  - Field cloning
  - MTI analysis
  - Message validation

- **[iso_8583_string_utils](utils/iso_8583_string_utils.md)** - String utilities
  - Padding
  - Trimming
  - Formatting

## 🚀 Quick Start

### Installation

Add to your `rebar.config`:

```erlang
{deps, [
    {iso_8583, {git, "https://github.com/your-user/iso_8583.git", {branch, "main"}}}
]}.
```

### Basic Example

```erlang
%% Create a message
Msg = iso_8583:new(),

%% Set MTI (0200 = Authorization request)
Msg1 = iso_8583:set_mti(<<"0200">>, Msg),

%% Add fields
Msg2 = iso_8583:set(2, <<"4111111111111111">>, Msg1),  % PAN
Msg3 = iso_8583:set(3, <<"000000">>, Msg2),            % Processing Code
Msg4 = iso_8583:set(4, <<"000000001000">>, Msg3),      % Amount
Msg5 = iso_8583:set(11, <<"123456">>, Msg4),           % STAN

%% Serialize to binary format
BinaryData = iso_8583_marshaller:marshal(Msg5, binary),

%% Send over network...

%% Deserialize response
Response = iso_8583_marshaller:unmarshal(ReceivedData, binary),

%% Check response code
RespCode = iso_8583:get(39, Response),
case RespCode of
    <<"00">> -> approved;
    <<"51">> -> insufficient_funds;
    _ -> other_error
end.
```

### Example with Utilities

```erlang
%% Receive request
Request = iso_8583_marshaller:unmarshal(Data, binary),

%% Validate
ok = iso_8583_message_utils:validate_mti(Request),
true = iso_8583_message_utils:is_request(Request),

%% Process transaction
ResultCode = process_transaction(Request),

%% Create response automatically
Response = iso_8583_message_utils:create_response_with_code(
    [2, 3, 4, 11, 41, 42],  % Fields to copy
    Request,
    ResultCode
),

%% Send response
ResponseData = iso_8583_marshaller:marshal(Response, binary).
```

## 📖 Guides

### By Use Case

#### Transaction Processing

1. **[Authorization Guide](guides/authorization.md)** - How to process authorizations
2. **[Reversal Guide](guides/reversals.md)** - How to handle reversals
3. **[Repeat Guide](guides/repeats.md)** - How to manage repeats

#### Integration

1. **[Network Integration](guides/network-integration.md)** - Connect with acquirers
2. **[Message Formats](guides/formats.md)** - Choose the right format
3. **[Versioning](guides/iso-versions.md)** - Work with different versions

#### Development

1. **[Testing](guides/testing.md)** - How to test your implementations
2. **[Debugging](guides/debugging.md)** - How to debug messages
3. **[Performance](guides/performance.md)** - Optimizations

## 🔍 References

### ISO 8583 Versions

The library supports three versions of the ISO 8583 standard:

| Version | Year | Fields | Characteristics |
|---------|------|--------|-----------------|
| **1987** | 1987 | 0-128 | Original version |
| **1993** | 1993 | 0-128 | Modified fields, EMV support |
| **2003** | 2003 | 0-192 | Extended fields, tertiary bitmap |

### Common MTI Codes

| MTI | Description |
|-----|-------------|
| 0100 | Authorization request |
| 0110 | Authorization response |
| 0200 | Financial request |
| 0210 | Financial response |
| 0400 | Reversal request |
| 0410 | Reversal response |
| 0800 | Network management |

### Common Response Codes

| Code | Description |
|------|-------------|
| 00 | Approved |
| 05 | Do not honor |
| 14 | Invalid card number |
| 51 | Insufficient funds |
| 54 | Expired card |
| 55 | Incorrect PIN |
| 91 | Issuer or switch inoperative |

## 🏗️ Architecture

```
src/
├── converters/          # Conversion modules
│   ├── iso_8583_hex_converter.erl
│   ├── iso_8583_numeric_converter.erl
│   ├── iso_8583_ebcdic_converter.erl
│   ├── iso_8583_bitmap_converter.erl
│   └── iso_8583_track2_converter.erl
│
├── fields/              # Field definitions by version
│   ├── iso_8583_fields_1987.erl
│   ├── iso_8583_fields_1993.erl
│   └── iso_8583_fields_2003.erl
│
├── marshallers/         # Marshallers by format
│   ├── iso_8583_marshaller_ascii.erl
│   ├── iso_8583_marshaller_binary.erl
│   ├── iso_8583_marshaller_ebcdic.erl
│   ├── iso_8583_marshaller_json.erl
│   └── iso_8583_marshaller_xml.erl
│
├── utils/               # Utilities
│   ├── iso_8583_message_utils.erl
│   └── iso_8583_string_utils.erl
│
├── iso_8583.erl              # Main message module
├── iso_8583_fields.erl       # Fields facade
├── iso_8583_converters.erl   # Converters facade
└── iso_8583_marshaller.erl   # Marshaller facade
```

## 🤝 Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a branch for your feature
3. Write tests
4. Submit a Pull Request

## 📝 License

Apache License 2.0

## 📧 Support

- **Issues**: [GitHub Issues](https://github.com/your-user/iso_8583/issues)
- **Discussions**: [GitHub Discussions](https://github.com/your-user/iso_8583/discussions)

## 🙏 Acknowledgments

Based on the original work of the erl8583 project.

---

**Last updated:** November 2025

## 🌐 Language

- [Português](../README.md) - Portuguese documentation
- **English** - You are here
