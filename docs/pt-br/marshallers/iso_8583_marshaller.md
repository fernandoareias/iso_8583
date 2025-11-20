# iso_8583_marshaller

## Descrição

Módulo facade para marshalling/unmarshalling de mensagens ISO 8583 em diferentes formatos. Fornece uma interface unificada e simplificada para serializar e desserializar mensagens ISO 8583.

## Formatos Suportados

- **ASCII** - Representação em string hexadecimal ASCII
- **Binary** - Formato binário compacto
- **EBCDIC** - Para sistemas mainframe IBM
- **JSON** - Representação JSON para APIs modernas
- **XML** - Representação XML para integrações enterprise
- **gRPC** - Para comunicação gRPC

## Funções Exportadas

### API Simplificada (Recomendada)

#### `marshal/2`

Serializa uma mensagem ISO 8583 usando um formato específico. Usa regras de encoding padrão baseadas na versão do MTI.

**Parâmetros:**
- `Message` - Mensagem ISO 8583
- `Format` - Formato de marshalling (átomo)

**Retorno:** `binary()` - Dados serializados

**Formatos válidos:** `ascii | binary | ebcdic | json | xml | grpc`

**Exemplo:**
```erlang
Msg = iso_8583:new(),
Msg1 = iso_8583:set_mti(<<"0200">>, Msg),
Msg2 = iso_8583:set(2, <<"4111111111111111">>, Msg1),

% Marshal para ASCII
AsciiData = iso_8583_marshaller:marshal(Msg2, ascii),

% Marshal para Binary
BinaryData = iso_8583_marshaller:marshal(Msg2, binary),

% Marshal para JSON
JsonData = iso_8583_marshaller:marshal(Msg2, json).
```

---

#### `marshal/3`

Serializa uma mensagem usando formato e regras de encoding específicas.

**Parâmetros:**
- `Message` - Mensagem ISO 8583
- `Format` - Formato de marshalling
- `EncodingRules` - Módulo de regras de encoding ou `undefined`

**Retorno:** `binary()`

**Exemplo:**
```erlang
% Forçar uso de regras ISO 8583:1987
Data = iso_8583_marshaller:marshal(Msg, binary, iso_8583_fields_1987).
```

---

#### `unmarshal/2`

Desserializa dados usando um formato específico. Detecta automaticamente as regras de encoding baseado no MTI.

**Parâmetros:**
- `Marshalled` - Dados serializados (binary ou list(byte()))
- `Format` - Formato usado na serialização

**Retorno:** `iso8583message()`

**Exemplo:**
```erlang
% Unmarshal de ASCII
Msg = iso_8583_marshaller:unmarshal(AsciiData, ascii).

% Unmarshal de Binary
Msg = iso_8583_marshaller:unmarshal(BinaryData, binary).

% Unmarshal de JSON
Msg = iso_8583_marshaller:unmarshal(JsonData, json).
```

---

#### `unmarshal/3`

Desserializa dados usando formato e regras de encoding específicas.

**Parâmetros:**
- `Marshalled` - Dados serializados
- `Format` - Formato
- `EncodingRules` - Módulo de regras ou `undefined`

**Retorno:** `iso8583message()`

**Exemplo:**
```erlang
Msg = iso_8583_marshaller:unmarshal(Data, binary, iso_8583_fields_2003).
```

---

### Funções de Configuração

#### `get_marshaller_config/1`

Retorna a configuração de handlers para um formato específico.

**Parâmetros:**
- `Format` - Formato de marshalling

**Retorno:** `list(marshal_handler())`

**Exemplo:**
```erlang
Config = iso_8583_marshaller:get_marshaller_config(ascii).
% Retorna lista de handlers para formato ASCII
```

---

#### `get_supported_formats/0`

Retorna lista de todos os formatos de marshalling suportados.

**Retorno:** `list(marshal_format())`

**Exemplo:**
```erlang
Formats = iso_8583_marshaller:get_supported_formats().
% Retorna: [ascii, binary, ebcdic, json, xml, grpc]
```

---

### API de Baixo Nível (Avançada)

#### `marshal_with_handlers/2`

Serializa uma mensagem usando handlers customizados.

**Parâmetros:**
- `Message` - Mensagem ISO 8583
- `MarshalHandlers` - Lista de handlers

**Retorno:** `binary()`

**Exemplo:**
```erlang
% Para casos avançados com handlers customizados
Handlers = iso_8583_marshaller:get_marshaller_config(ascii),
CustomHandlers = [{encoding_rules, meu_modulo_custom} | Handlers],
Data = iso_8583_marshaller:marshal_with_handlers(Msg, CustomHandlers).
```

---

#### `unmarshal_with_handlers/2`

Desserializa dados usando handlers customizados.

**Parâmetros:**
- `Marshalled` - Dados serializados
- `MarshalHandlers` - Lista de handlers

**Retorno:** `iso8583message()`

---

## Tipos de Handlers

### `marshal_handler()`

Tupla que especifica um módulo handler e sua função:

- `{field_marshaller, module()}` - Marshal/unmarshal de campos
- `{bitmap_marshaller, module()}` - Marshal/unmarshal de bitmap
- `{mti_marshaller, module()}` - Marshal/unmarshal de MTI
- `{init_marshaller, module()}` - Inicialização de marshal
- `{end_marshaller, module()}` - Finalização de marshal
- `{encoding_rules, module()}` - Regras de encoding de campos
- `{field_arranger, module()}` - Ordenação de campos

## Detecção Automática de Versão

O marshaller detecta automaticamente a versão ISO 8583 baseado no primeiro dígito do MTI:

| Dígito MTI | Versão | Regras Usadas |
|------------|--------|---------------|
| 0 | ISO 8583:1987 | `iso_8583_fields_1987` |
| 1 | ISO 8583:1993 | `iso_8583_fields_1993` |
| 2 | ISO 8583:2003 | `iso_8583_fields_2003` |

**Exemplo:**
```erlang
% MTI = 0200 -> Usa automaticamente regras 1987
% MTI = 1200 -> Usa automaticamente regras 1993
% MTI = 2200 -> Usa automaticamente regras 2003
```

## Exemplos de Uso

### Exemplo Básico

```erlang
% Criar mensagem
Msg = iso_8583:new(),
Msg1 = iso_8583:set_mti(<<"0200">>, Msg),
Msg2 = iso_8583:set(2, <<"4111111111111111">>, Msg1),
Msg3 = iso_8583:set(3, <<"000000">>, Msg2),
Msg4 = iso_8583:set(4, <<"000000001000">>, Msg3),

% Marshal para envio
BinaryData = iso_8583_marshaller:marshal(Msg4, binary),

% Enviar pela rede...
% Receber resposta...

% Unmarshal resposta
Response = iso_8583_marshaller:unmarshal(ReceivedData, binary).
```

### Exemplo com Diferentes Formatos

```erlang
% Receber em formato ASCII
AsciiMsg = iso_8583_marshaller:unmarshal(AsciiData, ascii),

% Converter para JSON (para log ou API)
JsonData = iso_8583_marshaller:marshal(AsciiMsg, json),

% Ou converter para Binary (para rede)
BinaryData = iso_8583_marshaller:marshal(AsciiMsg, binary).
```

### Exemplo com Versão Específica

```erlang
% Forçar uso de ISO 8583:2003
Msg2003 = iso_8583_marshaller:unmarshal(
    Data,
    binary,
    iso_8583_fields_2003
).

% Marshal com regras 1987
Data1987 = iso_8583_marshaller:marshal(
    Msg,
    ascii,
    iso_8583_fields_1987
).
```

### Exemplo de Integração Completa

```erlang
processar_transacao(Socket) ->
    % Receber dados da rede
    {ok, Data} = gen_tcp:recv(Socket, 0),

    % Unmarshal
    Request = iso_8583_marshaller:unmarshal(Data, binary),

    % Processar
    Response = processar(Request),

    % Marshal resposta
    ResponseData = iso_8583_marshaller:marshal(Response, binary),

    % Enviar
    gen_tcp:send(Socket, ResponseData).
```

## Módulos de Marshalling Específicos

Para uso direto de um formato específico, veja:

- [iso_8583_marshaller_ascii](iso_8583_marshaller_ascii.md)
- [iso_8583_marshaller_binary](iso_8583_marshaller_binary.md)
- [iso_8583_marshaller_ebcdic](iso_8583_marshaller_ebcdic.md)
- [iso_8583_marshaller_json](iso_8583_marshaller_json.md)
- [iso_8583_marshaller_xml](iso_8583_marshaller_xml.md)

## Veja Também

- [iso_8583](../modulos/iso_8583.md) - Módulo principal de mensagens
- [iso_8583_fields](../fields/iso_8583_fields.md) - Definições de campos por versão
- [Guia de Formatos](formatos.md) - Detalhes sobre cada formato
