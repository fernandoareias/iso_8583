# ISO 8583 - Erlang Library

A complete and modern Erlang library for working with ISO 8583 messages, the international standard for financial transactions.

## 🌟 Features

- ✅ **Complete Support**: ISO 8583:1987, 1993, and 2003
- ✅ **Multiple Formats**: ASCII, Binary, EBCDIC, JSON, XML, gRPC
- ✅ **Intuitive API**: Simple and clear interface
- ✅ **Type-Safe**: Complete Erlang specifications
- ✅ **Well Documented**: Complete documentation in Portuguese and English
- ✅ **Modular**: Well-organized architecture
- ✅ **Performant**: Optimized for high performance

## 📦 Installation

```bash
rebar3 compile
```

## 🚀 Quick Start

### Create and Send a Message

```erlang
%% Create authorization message
Msg = iso_8583:new(),
Msg1 = iso_8583:set_mti(<<"0200">>, Msg),
Msg2 = iso_8583:set(2, <<"4111111111111111">>, Msg1),

%% Serialize to binary
Data = iso_8583_marshaller:marshal(Msg2, binary).
```

## 📚 Documentation

Complete documentation is available in:
- 🇧🇷 [Português](https://github.com/fernandoareias/iso_8583/tree/main/docs/pt-br)
- 🇺🇸 [English](https://github.com/fernandoareias/iso_8583/tree/main/docs/en)

## 🏗️ Architecture

```
src/
├── converters/    # Converters
├── fields/        # Field definitions
├── marshallers/   # Marshallers
└── utils/         # Utilities
```

## 📝 License

Apache License 2.0

## 📧 Support

- **Issues**: [GitHub Issues](https://github.com/seu-usuario/iso_8583/issues)
- **Discussions**: [GitHub Discussions](https://github.com/seu-usuario/iso_8583/discussions)

## 🙏 Acknowledgments

Based on the original work of the erl8583 project.
