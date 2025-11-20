# Documentação ISO 8583 - Erlang

Bem-vindo à documentação completa da biblioteca ISO 8583 em Erlang. Esta biblioteca fornece um conjunto completo de ferramentas para trabalhar com mensagens ISO 8583, o padrão internacional para mensagens de transações financeiras.

## 📚 Índice

### Módulos Principais

- **[iso_8583](modulos/iso_8583.md)** - Módulo principal para criar e manipular mensagens ISO 8583
  - Criação de mensagens
  - Manipulação de campos
  - Gestão de atributos
  - MTI (Message Type Indicator)

### Definições de Campos

- **[iso_8583_fields](fields/iso_8583_fields.md)** - Facade para definições de campos (todas as versões)
- **[iso_8583_fields_1987](fields/iso_8583_fields_1987.md)** - Campos ISO 8583:1987
- **[iso_8583_fields_1993](fields/iso_8583_fields_1993.md)** - Campos ISO 8583:1993
- **[iso_8583_fields_2003](fields/iso_8583_fields_2003.md)** - Campos ISO 8583:2003

### Marshalling (Serialização)

- **[iso_8583_marshaller](marshallers/iso_8583_marshaller.md)** - Facade principal de marshalling
  - Suporta múltiplos formatos: ASCII, Binary, EBCDIC, JSON, XML, gRPC
  - Detecção automática de versão ISO
  - API simplificada e API avançada

#### Marshallers Específicos

- **[iso_8583_marshaller_ascii](marshallers/iso_8583_marshaller_ascii.md)** - Formato ASCII hexadecimal
- **[iso_8583_marshaller_binary](marshallers/iso_8583_marshaller_binary.md)** - Formato binário compacto
- **[iso_8583_marshaller_ebcdic](marshallers/iso_8583_marshaller_ebcdic.md)** - Formato EBCDIC (mainframes)
- **[iso_8583_marshaller_json](marshallers/iso_8583_marshaller_json.md)** - Formato JSON
- **[iso_8583_marshaller_xml](marshallers/iso_8583_marshaller_xml.md)** - Formato XML

### Conversores

- **[iso_8583_converters](converters/iso_8583_converters.md)** - Facade de conversores
- **[iso_8583_hex_converter](converters/iso_8583_hex_converter.md)** - Conversões hexadecimais
- **[iso_8583_numeric_converter](converters/iso_8583_numeric_converter.md)** - Conversões numéricas e BCD
- **[iso_8583_ebcdic_converter](converters/iso_8583_ebcdic_converter.md)** - Conversões EBCDIC/ASCII
- **[iso_8583_bitmap_converter](converters/iso_8583_bitmap_converter.md)** - Conversões de bitmap
- **[iso_8583_track2_converter](converters/iso_8583_track2_converter.md)** - Conversões Track 2

### Utilitários

- **[iso_8583_message_utils](utils/iso_8583_message_utils.md)** - Utilitários para mensagens
  - Criação de respostas
  - Manipulação de repeats
  - Clonagem de campos
  - Análise de MTI
  - Validação de mensagens

- **[iso_8583_string_utils](utils/iso_8583_string_utils.md)** - Utilitários de string
  - Padding
  - Trimming
  - Formatação

## 🚀 Início Rápido

### Instalação

Adicione ao seu `rebar.config`:

```erlang
{deps, [
    {iso_8583, {git, "https://github.com/seu-usuario/iso_8583.git", {branch, "main"}}}
]}.
```

### Exemplo Básico

```erlang
%% Criar uma mensagem
Msg = iso_8583:new(),

%% Definir MTI (0200 = Requisição de autorização)
Msg1 = iso_8583:set_mti(<<"0200">>, Msg),

%% Adicionar campos
Msg2 = iso_8583:set(2, <<"4111111111111111">>, Msg1),  % PAN
Msg3 = iso_8583:set(3, <<"000000">>, Msg2),            % Processing Code
Msg4 = iso_8583:set(4, <<"000000001000">>, Msg3),      % Amount
Msg5 = iso_8583:set(11, <<"123456">>, Msg4),           % STAN

%% Serializar para formato binário
BinaryData = iso_8583_marshaller:marshal(Msg5, binary),

%% Enviar pela rede...

%% Desserializar resposta
Response = iso_8583_marshaller:unmarshal(ReceivedData, binary),

%% Verificar código de resposta
RespCode = iso_8583:get(39, Response),
case RespCode of
    <<"00">> -> aprovado;
    <<"51">> -> fundos_insuficientes;
    _ -> outro_erro
end.
```

### Exemplo com Utilitários

```erlang
%% Receber requisição
Request = iso_8583_marshaller:unmarshal(Data, binary),

%% Validar
ok = iso_8583_message_utils:validate_mti(Request),
true = iso_8583_message_utils:is_request(Request),

%% Processar transação
ResultCode = processar_transacao(Request),

%% Criar resposta automaticamente
Response = iso_8583_message_utils:create_response_with_code(
    [2, 3, 4, 11, 41, 42],  % Campos a copiar
    Request,
    ResultCode
),

%% Enviar resposta
ResponseData = iso_8583_marshaller:marshal(Response, binary).
```

## 📖 Guias

### Por Caso de Uso

#### Processar Transações

1. **[Guia de Autorização](guias/autorizacao.md)** - Como processar autorizações
2. **[Guia de Reversas](guias/reversas.md)** - Como tratar reversas
3. **[Guia de Repeats](guias/repeats.md)** - Como gerenciar repeats

#### Integração

1. **[Integração com Redes](guias/integracao-redes.md)** - Conectar com adquirentes
2. **[Formato de Mensagens](guias/formatos.md)** - Escolher o formato correto
3. **[Versionamento](guias/versoes-iso.md)** - Trabalhar com diferentes versões

#### Desenvolvimento

1. **[Testes](guias/testes.md)** - Como testar suas implementações
2. **[Debug](guias/debug.md)** - Como debugar mensagens
3. **[Performance](guias/performance.md)** - Otimizações

## 🔍 Referências

### Versões ISO 8583

A biblioteca suporta três versões do padrão ISO 8583:

| Versão | Ano | Campos | Características |
|--------|-----|--------|-----------------|
| **1987** | 1987 | 0-128 | Versão original |
| **1993** | 1993 | 0-128 | Campos modificados, suporte EMV |
| **2003** | 2003 | 0-192 | Campos estendidos, tertiary bitmap |

### Códigos MTI Comuns

| MTI | Descrição |
|-----|-----------|
| 0100 | Requisição de autorização |
| 0110 | Resposta de autorização |
| 0200 | Requisição financeira |
| 0210 | Resposta financeira |
| 0400 | Requisição de reversa |
| 0410 | Resposta de reversa |
| 0800 | Network management |

### Códigos de Resposta Comuns

| Código | Descrição |
|--------|-----------|
| 00 | Aprovado |
| 05 | Não honrar |
| 14 | Número de cartão inválido |
| 51 | Fundos insuficientes |
| 54 | Cartão expirado |
| 55 | Senha incorreta |
| 91 | Emissor ou switch inoperante |

## 🏗️ Arquitetura

```
src/
├── converters/          # Módulos de conversão
│   ├── iso_8583_hex_converter.erl
│   ├── iso_8583_numeric_converter.erl
│   ├── iso_8583_ebcdic_converter.erl
│   ├── iso_8583_bitmap_converter.erl
│   └── iso_8583_track2_converter.erl
│
├── fields/              # Definições de campos por versão
│   ├── iso_8583_fields_1987.erl
│   ├── iso_8583_fields_1993.erl
│   └── iso_8583_fields_2003.erl
│
├── marshallers/         # Marshallers por formato
│   ├── iso_8583_marshaller_ascii.erl
│   ├── iso_8583_marshaller_binary.erl
│   ├── iso_8583_marshaller_ebcdic.erl
│   ├── iso_8583_marshaller_json.erl
│   └── iso_8583_marshaller_xml.erl
│
├── utils/               # Utilitários
│   ├── iso_8583_message_utils.erl
│   └── iso_8583_string_utils.erl
│
├── iso_8583.erl              # Módulo principal de mensagens
├── iso_8583_fields.erl       # Facade de campos
├── iso_8583_converters.erl   # Facade de conversores
└── iso_8583_marshaller.erl   # Facade de marshalling
```

## 🤝 Contribuindo

Contribuições são bem-vindas! Por favor:

1. Fork o repositório
2. Crie uma branch para sua feature
3. Escreva testes
4. Envie um Pull Request

## 📝 Licença

Apache License 2.0

## 📧 Suporte

- **Issues**: [GitHub Issues](https://github.com/seu-usuario/iso_8583/issues)
- **Discussões**: [GitHub Discussions](https://github.com/seu-usuario/iso_8583/discussions)

## 🙏 Agradecimentos

Baseado no trabalho original do projeto erl8583.

---

**Última atualização:** Novembro 2025

## 🌐 Idiomas

- **Português** - Você está aqui
- [English](en/README.md) - English documentation
