# iso_8583_fields

## Description

Facade module that provides a unified interface to access ISO 8583 field encoding definitions across different versions of the standard (1987, 1993, 2003).

By default, uses version 1993 for backward compatibility, but allows explicitly specifying which version to use.

## Supported Versions

- **ISO 8583:1987** - Original version of the standard
- **ISO 8583:1993** - Version with improvements and EMV support
- **ISO 8583:2003** - Modern version with extended fields (0-192)

## Exported Functions

### Field Encoding

#### `get_encoding/1`

Returns field encoding using the default version (1993).

**Parameters:**
- `FieldId` - Field ID (integer) or list of integers for subfields

**Return:** `field_encoding()` - Tuple `{Type, Format, MaxSize}`

**Types:**
- `n` - Numeric
- `an` - Alphanumeric
- `ans` - Alphanumeric and special
- `b` - Binary
- `z` - Track data
- `x_n` - Numeric with sign

**Formats:**
- `fixed` - Fixed size
- `llvar` - Variable with 2-digit size
- `lllvar` - Variable with 3-digit size

**Example:**
```erlang
% Field 2 (PAN) - Numeric, variable (LL), up to 19 digits
{n, llvar, 19} = iso_8583_fields:get_encoding(2).

% Field 4 (Amount) - Numeric, fixed, 12 digits
{n, fixed, 12} = iso_8583_fields:get_encoding(4).

% Field 55 (EMV Data) - ANS, variable (LLL), up to 999 bytes
{ans, lllvar, 999} = iso_8583_fields:get_encoding(55).
```

---

#### `get_encoding/2`

Returns field encoding for a specific ISO 8583 version.

**Parameters:**
- `FieldId` - Field ID
- `Version` - ISO version: `'1987' | '1993' | '2003'`

**Return:** `field_encoding()`

**Example:**
```erlang
% Field 12 in 1987 version (6 digits - time only)
{n, fixed, 6} = iso_8583_fields:get_encoding(12, '1987').

% Field 12 in 1993 version (12 digits - date and time)
{n, fixed, 12} = iso_8583_fields:get_encoding(12, '1993').

% Field 12 in 2003 version (14 digits - extended)
{n, fixed, 14} = iso_8583_fields:get_encoding(12, '2003').
```

---

### Field Metadata

#### `get_field_name/1`

Returns the descriptive name of a field using the default version.

**Parameters:**
- `FieldId` - Field ID

**Return:** `binary()` - Field name

**Example:**
```erlang
<<"Primary Account Number">> = iso_8583_fields:get_field_name(2).
<<"Response Code">> = iso_8583_fields:get_field_name(39).
<<"Systems Trace Audit Number">> = iso_8583_fields:get_field_name(11).
```

---

#### `get_field_name/2`

Returns the field name for a specific version.

**Parameters:**
- `FieldId` - Field ID
- `Version` - ISO version

**Return:** `binary()`

**Example:**
```erlang
% Field 22 has different names across versions
<<"POS Entry Mode">> = iso_8583_fields:get_field_name(22, '1987').
<<"POS Data Code">> = iso_8583_fields:get_field_name(22, '1993').
```

---

### Validation

#### `is_valid_field/1`

Checks if a field ID is valid in the default version.

**Parameters:**
- `FieldId` - Field ID

**Return:** `boolean()`

**Example:**
```erlang
true = iso_8583_fields:is_valid_field(2).
true = iso_8583_fields:is_valid_field(128).
false = iso_8583_fields:is_valid_field(200).
```

---

#### `is_valid_field/2`

Checks if an ID is valid for a specific version.

**Parameters:**
- `FieldId` - Field ID
- `Version` - ISO version

**Return:** `boolean()`

**Example:**
```erlang
% Field 150 is only valid in ISO 2003
false = iso_8583_fields:is_valid_field(150, '1987').
false = iso_8583_fields:is_valid_field(150, '1993').
true = iso_8583_fields:is_valid_field(150, '2003').
```

---

### Utilities

#### `get_all_fields/0`

Returns list of all field IDs in the default version.

**Return:** `list(integer())`

**Example:**
```erlang
AllFields = iso_8583_fields:get_all_fields().
% [0, 1, 2, 3, ..., 127, 128]
```

---

#### `get_all_fields/1`

Returns list of IDs for a specific version.

**Parameters:**
- `Version` - ISO version

**Return:** `list(integer())`

**Example:**
```erlang
Fields1987 = iso_8583_fields:get_all_fields('1987').
% [0..128]

Fields2003 = iso_8583_fields:get_all_fields('2003').
% [0..192] - Includes extended fields
```

---

#### `get_supported_versions/0`

Returns list of supported ISO versions.

**Return:** `list(iso_version())`

**Example:**
```erlang
Versions = iso_8583_fields:get_supported_versions().
% ['1987', '1993', '2003']
```

---

#### `get_default_version/0`

Returns the default ISO version used by the module.

**Return:** `iso_version()`

**Example:**
```erlang
'1993' = iso_8583_fields:get_default_version().
```

---

## Differences Between Versions

### Fields Modified in 1993

| Field | Name 1987 | Name 1993 |
|-------|-----------|-----------|
| 12 | Time, Local Transaction (6) | Date and Time, Local Transaction (12) |
| 13 | Date, Local Transaction (4) | Date, Effective (4) |
| 15 | Date, Settlement (4) | Date, Settlement (6) |
| 22 | POS Entry Mode (3) | POS Data Code (12) |
| 24 | Network International ID (3) | Function Code (3) |
| 25 | POS Condition Code (2) | Message Reason Code (4) |
| 55 | Reserved ISO 1 | ICC System Related Data (EMV) |

### Extended Fields in 2003

- **Fields 129-192**: Available only in ISO 2003
- **Field 65**: Tertiary bitmap to access fields 129-192
- **Modified fields**: 12 (14 digits), 53 (48 chars), 54 (255 chars)

## Usage Examples

### Basic Example

```erlang
% Get field encoding
{Type, Format, MaxLen} = iso_8583_fields:get_encoding(2),
% {n, llvar, 19} - Numeric, variable, up to 19

% Use encoding for validation
validate_field(FieldId, Value) ->
    {Type, Format, MaxLen} = iso_8583_fields:get_encoding(FieldId),
    case Format of
        fixed -> byte_size(Value) =:= MaxLen;
        llvar -> byte_size(Value) =< MaxLen;
        lllvar -> byte_size(Value) =< MaxLen
    end.
```

### Multi-Version Example

```erlang
% Process message based on version
process_field_12(Msg, Version) ->
    {_Type, _Format, MaxLen} = iso_8583_fields:get_encoding(12, Version),
    DateTime = iso_8583:get(12, Msg),

    case MaxLen of
        6 -> parse_time(DateTime);              % 1987
        12 -> parse_datetime(DateTime);         % 1993
        14 -> parse_datetime_extended(DateTime) % 2003
    end.
```

### Validation Example

```erlang
validate_message(Msg, Version) ->
    Fields = iso_8583:get_fields(Msg),

    % Check if all fields are valid for the version
    lists:all(
        fun(FieldId) ->
            iso_8583_fields:is_valid_field(FieldId, Version)
        end,
        Fields
    ).
```

### Introspection Example

```erlang
list_message_fields(Msg) ->
    Fields = iso_8583:get_fields(Msg),

    lists:map(
        fun(FieldId) ->
            Name = iso_8583_fields:get_field_name(FieldId),
            Value = iso_8583:get(FieldId, Msg),
            {Encoding, Format, MaxLen} = iso_8583_fields:get_encoding(FieldId),

            #{
                id => FieldId,
                name => Name,
                value => Value,
                type => Encoding,
                format => Format,
                max_length => MaxLen
            }
        end,
        Fields
    ).
```

## Common Fields

### Identification Fields

| Field | Name | Encoding | Usage |
|-------|------|----------|-------|
| 0 | MTI | n, fixed, 4 | Message Type Indicator |
| 2 | PAN | n, llvar, 19 | Primary Account Number |
| 11 | STAN | n, fixed, 6 | Systems Trace Audit Number |
| 41 | Terminal ID | ans, fixed, 8 | Card Acceptor Terminal ID |
| 42 | Merchant ID | ans, fixed, 15 | Card Acceptor ID Code |

### Transaction Fields

| Field | Name | Encoding | Usage |
|-------|------|----------|-------|
| 3 | Processing Code | n, fixed, 6 | Transaction type |
| 4 | Amount | n, fixed, 12 | Transaction amount |
| 7 | DateTime | n, fixed, 10 | Transmission Date/Time |
| 12 | Local DateTime | n, fixed, 12/14 | Local date/time |
| 49 | Currency Code | an, fixed, 3 | Currency code |

### Response Fields

| Field | Name | Encoding | Usage |
|-------|------|----------|-------|
| 38 | Approval Code | an, fixed, 6 | Approval code |
| 39 | Response Code | an, fixed, 2/3 | Response code |

## See Also

- [iso_8583_fields_1987](iso_8583_fields_1987.md) - ISO 8583:1987 fields
- [iso_8583_fields_1993](iso_8583_fields_1993.md) - ISO 8583:1993 fields
- [iso_8583_fields_2003](iso_8583_fields_2003.md) - ISO 8583:2003 fields
- [iso_8583](../modules/iso_8583.md) - Message module
