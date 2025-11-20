# iso_8583_fields

## Descrição

Módulo facade que fornece interface unificada para acessar definições de encoding de campos ISO 8583 em diferentes versões do padrão (1987, 1993, 2003).

Por padrão, usa a versão 1993 para retrocompatibilidade, mas permite especificar explicitamente qual versão usar.

## Versões Suportadas

- **ISO 8583:1987** - Versão original do padrão
- **ISO 8583:1993** - Versão com melhorias e suporte EMV
- **ISO 8583:2003** - Versão moderna com campos estendidos (0-192)

## Funções Exportadas

### Encoding de Campos

#### `get_encoding/1`

Retorna o encoding de um campo usando a versão padrão (1993).

**Parâmetros:**
- `FieldId` - ID do campo (inteiro) ou lista de inteiros para subcampos

**Retorno:** `field_encoding()` - Tupla `{Tipo, Formato, TamanhoMax}`

**Tipos:**
- `n` - Numérico
- `an` - Alfanumérico
- `ans` - Alfanumérico e especiais
- `b` - Binário
- `z` - Track data
- `x_n` - Numérico com sinal

**Formatos:**
- `fixed` - Tamanho fixo
- `llvar` - Variável com 2 dígitos de tamanho
- `lllvar` - Variável com 3 dígitos de tamanho

**Exemplo:**
```erlang
% Campo 2 (PAN) - Numérico, variável (LL), até 19 dígitos
{n, llvar, 19} = iso_8583_fields:get_encoding(2).

% Campo 4 (Amount) - Numérico, fixo, 12 dígitos
{n, fixed, 12} = iso_8583_fields:get_encoding(4).

% Campo 55 (EMV Data) - ANS, variável (LLL), até 999 bytes
{ans, lllvar, 999} = iso_8583_fields:get_encoding(55).
```

---

#### `get_encoding/2`

Retorna o encoding de um campo para uma versão específica do ISO 8583.

**Parâmetros:**
- `FieldId` - ID do campo
- `Version` - Versão ISO: `'1987' | '1993' | '2003'`

**Retorno:** `field_encoding()`

**Exemplo:**
```erlang
% Campo 12 na versão 1987 (6 dígitos - apenas hora)
{n, fixed, 6} = iso_8583_fields:get_encoding(12, '1987').

% Campo 12 na versão 1993 (12 dígitos - data e hora)
{n, fixed, 12} = iso_8583_fields:get_encoding(12, '1993').

% Campo 12 na versão 2003 (14 dígitos - estendido)
{n, fixed, 14} = iso_8583_fields:get_encoding(12, '2003').
```

---

### Metadados de Campos

#### `get_field_name/1`

Retorna o nome descritivo de um campo usando a versão padrão.

**Parâmetros:**
- `FieldId` - ID do campo

**Retorno:** `binary()` - Nome do campo

**Exemplo:**
```erlang
<<"Primary Account Number">> = iso_8583_fields:get_field_name(2).
<<"Response Code">> = iso_8583_fields:get_field_name(39).
<<"Systems Trace Audit Number">> = iso_8583_fields:get_field_name(11).
```

---

#### `get_field_name/2`

Retorna o nome do campo para uma versão específica.

**Parâmetros:**
- `FieldId` - ID do campo
- `Version` - Versão ISO

**Retorno:** `binary()`

**Exemplo:**
```erlang
% Campo 22 tem nomes diferentes entre versões
<<"POS Entry Mode">> = iso_8583_fields:get_field_name(22, '1987').
<<"POS Data Code">> = iso_8583_fields:get_field_name(22, '1993').
```

---

### Validação

#### `is_valid_field/1`

Verifica se um ID de campo é válido na versão padrão.

**Parâmetros:**
- `FieldId` - ID do campo

**Retorno:** `boolean()`

**Exemplo:**
```erlang
true = iso_8583_fields:is_valid_field(2).
true = iso_8583_fields:is_valid_field(128).
false = iso_8583_fields:is_valid_field(200).
```

---

#### `is_valid_field/2`

Verifica se um ID é válido para uma versão específica.

**Parâmetros:**
- `FieldId` - ID do campo
- `Version` - Versão ISO

**Retorno:** `boolean()`

**Exemplo:**
```erlang
% Campo 150 só é válido em ISO 2003
false = iso_8583_fields:is_valid_field(150, '1987').
false = iso_8583_fields:is_valid_field(150, '1993').
true = iso_8583_fields:is_valid_field(150, '2003').
```

---

### Utilitários

#### `get_all_fields/0`

Retorna lista de todos os IDs de campos da versão padrão.

**Retorno:** `list(integer())`

**Exemplo:**
```erlang
AllFields = iso_8583_fields:get_all_fields().
% [0, 1, 2, 3, ..., 127, 128]
```

---

#### `get_all_fields/1`

Retorna lista de IDs para uma versão específica.

**Parâmetros:**
- `Version` - Versão ISO

**Retorno:** `list(integer())`

**Exemplo:**
```erlang
Fields1987 = iso_8583_fields:get_all_fields('1987').
% [0..128]

Fields2003 = iso_8583_fields:get_all_fields('2003').
% [0..192] - Inclui campos estendidos
```

---

#### `get_supported_versions/0`

Retorna lista de versões ISO suportadas.

**Retorno:** `list(iso_version())`

**Exemplo:**
```erlang
Versions = iso_8583_fields:get_supported_versions().
% ['1987', '1993', '2003']
```

---

#### `get_default_version/0`

Retorna a versão ISO padrão usada pelo módulo.

**Retorno:** `iso_version()`

**Exemplo:**
```erlang
'1993' = iso_8583_fields:get_default_version().
```

---

## Diferenças Entre Versões

### Campos Modificados em 1993

| Campo | Nome 1987 | Nome 1993 |
|-------|-----------|-----------|
| 12 | Time, Local Transaction (6) | Date and Time, Local Transaction (12) |
| 13 | Date, Local Transaction (4) | Date, Effective (4) |
| 15 | Date, Settlement (4) | Date, Settlement (6) |
| 22 | POS Entry Mode (3) | POS Data Code (12) |
| 24 | Network International ID (3) | Function Code (3) |
| 25 | POS Condition Code (2) | Message Reason Code (4) |
| 55 | Reserved ISO 1 | ICC System Related Data (EMV) |

### Campos Estendidos em 2003

- **Campos 129-192**: Disponíveis apenas em ISO 2003
- **Campo 65**: Tertiary bitmap para acessar campos 129-192
- **Campos modificados**: 12 (14 dígitos), 53 (48 chars), 54 (255 chars)

## Exemplos de Uso

### Exemplo Básico

```erlang
% Obter encoding de campo
{Type, Format, MaxLen} = iso_8583_fields:get_encoding(2),
% {n, llvar, 19} - Numérico, variável, até 19

% Usar encoding para validação
validar_campo(FieldId, Value) ->
    {Type, Format, MaxLen} = iso_8583_fields:get_encoding(FieldId),
    case Format of
        fixed -> byte_size(Value) =:= MaxLen;
        llvar -> byte_size(Value) =< MaxLen;
        lllvar -> byte_size(Value) =< MaxLen
    end.
```

### Exemplo Multi-Versão

```erlang
% Processar mensagem baseado na versão
processar_campo_12(Msg, Version) ->
    {_Type, _Format, MaxLen} = iso_8583_fields:get_encoding(12, Version),
    DateTime = iso_8583:get(12, Msg),

    case MaxLen of
        6 -> parsear_hora(DateTime);           % 1987
        12 -> parsear_data_hora(DateTime);     % 1993
        14 -> parsear_data_hora_ext(DateTime)  % 2003
    end.
```

### Exemplo de Validação

```erlang
validar_mensagem(Msg, Version) ->
    Fields = iso_8583:get_fields(Msg),

    % Verificar se todos os campos são válidos para a versão
    lists:all(
        fun(FieldId) ->
            iso_8583_fields:is_valid_field(FieldId, Version)
        end,
        Fields
    ).
```

### Exemplo de Introspecção

```erlang
listar_campos_mensagem(Msg) ->
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

## Campos Comuns

### Campos de Identificação

| Campo | Nome | Encoding | Uso |
|-------|------|----------|-----|
| 0 | MTI | n, fixed, 4 | Message Type Indicator |
| 2 | PAN | n, llvar, 19 | Primary Account Number |
| 11 | STAN | n, fixed, 6 | Systems Trace Audit Number |
| 41 | Terminal ID | ans, fixed, 8 | Card Acceptor Terminal ID |
| 42 | Merchant ID | ans, fixed, 15 | Card Acceptor ID Code |

### Campos de Transação

| Campo | Nome | Encoding | Uso |
|-------|------|----------|-----|
| 3 | Processing Code | n, fixed, 6 | Tipo de transação |
| 4 | Amount | n, fixed, 12 | Valor da transação |
| 7 | DateTime | n, fixed, 10 | Transmission Date/Time |
| 12 | Local DateTime | n, fixed, 12/14 | Data/Hora local |
| 49 | Currency Code | an, fixed, 3 | Código da moeda |

### Campos de Resposta

| Campo | Nome | Encoding | Uso |
|-------|------|----------|-----|
| 38 | Approval Code | an, fixed, 6 | Código de aprovação |
| 39 | Response Code | an, fixed, 2/3 | Código de resposta |

## Veja Também

- [iso_8583_fields_1987](iso_8583_fields_1987.md) - Campos ISO 8583:1987
- [iso_8583_fields_1993](iso_8583_fields_1993.md) - Campos ISO 8583:1993
- [iso_8583_fields_2003](iso_8583_fields_2003.md) - Campos ISO 8583:2003
- [iso_8583](../modulos/iso_8583.md) - Módulo de mensagens
