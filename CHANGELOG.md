# Changelog

All notable changes to this project will be documented in this file.

## [1.0.0] - 2025-11-20

### Added

#### Modular Structure
- Separation of modules by responsibility:
  - `src/converters/` - Specialized conversion modules
  - `src/fields/` - Field definitions by ISO version
  - `src/marshallers/` - Serializers by format
  - `src/utils/` - Auxiliary utilities

#### Converters
- `iso_8583_hex_converter` - Hexadecimal conversions
- `iso_8583_numeric_converter` - Numeric and BCD conversions
- `iso_8583_ebcdic_converter` - EBCDIC/ASCII conversions
- `iso_8583_bitmap_converter` - Bitmap conversions
- `iso_8583_track2_converter` - Track 2 conversions
- `iso_8583_converters` - Facade for all converters

#### Fields
- `iso_8583_fields_1987` - Complete ISO 8583:1987 definitions
- `iso_8583_fields_1993` - ISO 8583:1993 definitions with inheritance from 1987
- `iso_8583_fields_2003` - ISO 8583:2003 definitions with extended fields (0-192)
- `iso_8583_fields` - Facade for all versions

#### Marshallers
- `iso_8583_marshaller_ascii` - ASCII hexadecimal format
- `iso_8583_marshaller_binary` - Compact binary format
- `iso_8583_marshaller_ebcdic` - EBCDIC format
- `iso_8583_marshaller_json` - JSON format
- `iso_8583_marshaller_xml` - XML format
- `iso_8583_marshaller` - Facade with simplified API and automatic version detection

#### Utils
- `iso_8583_message_utils` - 20+ utility functions:
  - Response creation (4 variants)
  - Repeat management
  - Field operations (clone, copy)
  - MTI analysis (7 functions)
  - Message validation
- `iso_8583_string_utils` - String utilities (padding, trimming)

#### Documentation
- Complete documentation in Portuguese at `docs/`
- Complete documentation in English at `docs/en/`
- Per-module documentation with practical examples
- Reference guides for MTI, response codes, ISO versions
- Usage examples in `examples/`

#### Build Tools
- `.app.src` file with all modules listed
- Optimized `rebar.config` with:
  - Warnings as errors
  - Dialyzer configuration
  - EDoc configuration
  - Directory structure
- `Makefile` with useful targets (compile, test, docs, etc.)
- Appropriate `.gitignore` for Erlang projects

### Improved

#### Main API (`iso_8583`)
- Added `get/3` function with default value
- Added `has_field/2` function to check existence
- Added `get_attribute/3` function with default value
- Fixed bug in `remove/2` with self-reference

#### Marshaller Facade
- Simplified API with formats as atoms
- Automatic ISO version detection based on MTI
- Support for multiple formats: ascii, binary, ebcdic, json, xml, grpc

#### Organization
- Code organized in directories by functionality
- Facade pattern applied consistently
- Clear separation of responsibilities

### Removed
- `iso_8583_message_helpers.erl` - Replaced by `iso_8583_message_utils.erl`
- `iso_8583_convert.erl` - Broken into specialized modules

## Migration Notes

### From erl8583 to iso_8583

1. **Imports**: Update module references:
   ```erlang
   % Before
   erl8583_message:new()

   % After
   iso_8583:new()
   ```

2. **Converters**: Use facade or specialized modules:
   ```erlang
   % Before
   iso_8583_convert:hex_to_binary(...)

   % After
   iso_8583_converters:hex_to_binary(...)
   % or
   iso_8583_hex_converter:hex_to_binary(...)
   ```

3. **Marshalling**: Use new simplified API:
   ```erlang
   % Before
   Handlers = [...],
   Data = marshal(Msg, Handlers)

   % After
   Data = iso_8583_marshaller:marshal(Msg, binary)
   ```

4. **Helpers**: Use message_utils:
   ```erlang
   % Before
   iso_8583_message_helpers:create_response(...)

   % After
   iso_8583_message_utils:create_response(...)
   ```

## Compatibility

- Erlang/OTP 24 or higher
- All three ISO 8583 versions (1987, 1993, 2003)
- Multiple serialization formats
