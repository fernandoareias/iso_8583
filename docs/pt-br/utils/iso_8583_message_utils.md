# iso_8583_message_utils

## Descrição

Módulo de utilitários para trabalhar com mensagens ISO 8583. Fornece funções auxiliares para criar respostas, manipular repeats, clonar campos, analisar MTI e validar mensagens.

## Funções Exportadas

### Criação de Respostas

#### `create_response/1`

Cria uma mensagem de resposta com os mesmos valores de campos da mensagem original. Automaticamente altera o MTI para indicar que é uma resposta e limpa a flag de repeat.

**Parâmetros:**
- `Message` - Mensagem ISO 8583 original (requisição)

**Retorno:** `iso8583message()` - Mensagem de resposta

**Exemplo:**
```erlang
Request = iso_8583_marshaller:unmarshal(Data, binary),
Response = iso_8583_message_utils:create_response(Request).
```

---

#### `create_response/2`

Cria uma mensagem de resposta copiando apenas os campos especificados da mensagem original. Útil quando você quer retornar apenas um subconjunto de campos na resposta.

**Parâmetros:**
- `FieldIds` - Lista de IDs dos campos a copiar
- `Message` - Mensagem ISO 8583 original

**Retorno:** `iso8583message()`

**Exemplo:**
```erlang
% Copiar apenas campos essenciais para resposta
Response = iso_8583_message_utils:create_response(
    [2, 3, 4, 11, 41, 42],  % PAN, ProcCode, Amount, STAN, Terminal, Merchant
    Request
).
```

---

#### `create_response_with_code/2`

Cria uma mensagem de resposta com um código de resposta específico. O código de resposta é definido no campo 39 (RESP_CODE).

**Parâmetros:**
- `Message` - Mensagem ISO 8583 original
- `ResponseCode` - Código de resposta (binary)

**Retorno:** `iso8583message()`

**Exemplo:**
```erlang
% Criar resposta aprovada
Response = iso_8583_message_utils:create_response_with_code(
    Request,
    <<"00">>  % Aprovado
).

% Criar resposta negada
Response = iso_8583_message_utils:create_response_with_code(
    Request,
    <<"51">>  % Fundos insuficientes
).
```

---

#### `create_response_with_code/3`

Cria uma mensagem de resposta com campos específicos e código de resposta.

**Parâmetros:**
- `FieldIds` - Lista de IDs dos campos a copiar
- `Message` - Mensagem ISO 8583 original
- `ResponseCode` - Código de resposta

**Retorno:** `iso8583message()`

**Exemplo:**
```erlang
Response = iso_8583_message_utils:create_response_with_code(
    [2, 3, 4, 11, 41, 42],
    Request,
    <<"00">>
).
```

---

### Gestão de Repeats

#### `mark_as_repeat/1`

Marca uma mensagem como repeat atualizando o MTI. Altera o último dígito do MTI de acordo com as regras ISO 8583 de repeat.

**Regras de conversão:**
- `0` → `1` (Requisição → Repeat requisição)
- `2` → `3` (Resposta → Repeat resposta)
- `4` → `5` (Advice → Repeat advice)

**Parâmetros:**
- `Message` - Mensagem ISO 8583

**Retorno:** `iso8583message()`

**Exemplo:**
```erlang
RepeatMsg = iso_8583_message_utils:mark_as_repeat(OriginalMsg).
```

---

#### `is_repeat/1`

Verifica se uma mensagem está marcada como repeat.

**Parâmetros:**
- `Message` - Mensagem ISO 8583

**Retorno:** `boolean()`

**Exemplo:**
```erlang
case iso_8583_message_utils:is_repeat(Msg) of
    true -> processar_como_repeat(Msg);
    false -> processar_normalmente(Msg)
end.
```

---

### Operações de Campos

#### `clone_fields/1`

Clona todos os campos de uma mensagem para uma nova mensagem.

**Parâmetros:**
- `Message` - Mensagem fonte

**Retorno:** `iso8583message()`

**Exemplo:**
```erlang
Clone = iso_8583_message_utils:clone_fields(Original).
```

---

#### `clone_fields/2`

Clona campos específicos de uma mensagem fonte para uma nova mensagem.

**Parâmetros:**
- `FieldIds` - Lista de IDs dos campos a clonar
- `Message` - Mensagem fonte

**Retorno:** `iso8583message()`

**Exemplo:**
```erlang
% Clonar apenas campos de identificação
Clone = iso_8583_message_utils:clone_fields([2, 11, 41, 42], Original).
```

---

#### `clone_fields/3`

Clona campos específicos da mensagem fonte para a mensagem destino.

**Parâmetros:**
- `FieldIds` - Lista de IDs dos campos
- `Source` - Mensagem fonte
- `Target` - Mensagem destino

**Retorno:** `iso8583message()` - Mensagem destino atualizada

**Exemplo:**
```erlang
Updated = iso_8583_message_utils:clone_fields(
    [2, 3, 4],
    SourceMsg,
    TargetMsg
).
```

---

#### `copy_fields/3`

Copia campos específicos de uma mensagem para outra (in-place). Similar a `clone_fields/3`.

**Parâmetros:**
- `FieldIds` - Lista de IDs
- `Source` - Mensagem fonte
- `Target` - Mensagem destino

**Retorno:** `iso8583message()`

---

### Análise de MTI

#### `get_mti_type/1`

Extrai o tipo de mensagem do MTI (posição 3).

**Parâmetros:**
- `Message` - Mensagem ISO 8583

**Retorno:** `request | response | advice | notification | unknown`

**Exemplos de MTI:**
- `0200` → `request` (requisição de autorização)
- `0210` → `response` (resposta de autorização)
- `0420` → `advice` (advice de reversa)
- `0810` → `notification` (network management)

**Exemplo:**
```erlang
case iso_8583_message_utils:get_mti_type(Msg) of
    request -> processar_requisicao(Msg);
    response -> processar_resposta(Msg);
    advice -> processar_advice(Msg);
    _ -> {error, tipo_desconhecido}
end.
```

---

#### `get_mti_version/1`

Extrai a versão ISO 8583 do MTI (posição 1).

**Parâmetros:**
- `Message` - Mensagem ISO 8583

**Retorno:** `'1987' | '1993' | '2003' | unknown`

**Exemplos de MTI:**
- `0200` → `'1987'`
- `1200` → `'1993'`
- `2200` → `'2003'`

**Exemplo:**
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

Verifica se a mensagem é uma requisição.

**Parâmetros:**
- `Message` - Mensagem ISO 8583

**Retorno:** `boolean()`

---

#### `is_response/1`

Verifica se a mensagem é uma resposta.

**Parâmetros:**
- `Message` - Mensagem ISO 8583

**Retorno:** `boolean()`

---

#### `is_advice/1`

Verifica se a mensagem é um advice.

**Parâmetros:**
- `Message` - Mensagem ISO 8583

**Retorno:** `boolean()`

---

#### `is_reversal/1`

Verifica se a mensagem é uma reversa (baseado no processing code).

**Parâmetros:**
- `Message` - Mensagem ISO 8583

**Retorno:** `boolean()`

**Nota:** Identifica reversas verificando se o processing code (campo 3) começa com "04".

**Exemplo:**
```erlang
case iso_8583_message_utils:is_reversal(Msg) of
    true -> processar_reversa(Msg);
    false -> processar_transacao_normal(Msg)
end.
```

---

### Validação de Mensagens

#### `has_required_fields/2`

Verifica se todos os campos obrigatórios estão presentes na mensagem.

**Parâmetros:**
- `RequiredFields` - Lista de IDs dos campos obrigatórios
- `Message` - Mensagem ISO 8583

**Retorno:** `boolean()`

**Exemplo:**
```erlang
RequiredFields = [2, 3, 4, 11, 41, 42],
case iso_8583_message_utils:has_required_fields(RequiredFields, Msg) of
    true -> processar(Msg);
    false -> {error, campos_obrigatorios_ausentes}
end.
```

---

#### `validate_mti/1`

Valida o formato do MTI (deve ter 4 dígitos).

**Parâmetros:**
- `Message` - Mensagem ISO 8583

**Retorno:** `ok | {error, invalid_mti}`

**Exemplo:**
```erlang
case iso_8583_message_utils:validate_mti(Msg) of
    ok -> processar(Msg);
    {error, invalid_mti} -> rejeitar(Msg)
end.
```

---

## Exemplo Completo

```erlang
%% Processar requisição e criar resposta
processar_requisicao(Data) ->
    % Unmarshal mensagem
    Request = iso_8583_marshaller:unmarshal(Data, binary),

    % Validar MTI
    ok = iso_8583_message_utils:validate_mti(Request),

    % Verificar se é requisição
    true = iso_8583_message_utils:is_request(Request),

    % Verificar campos obrigatórios
    CamposObrigatorios = [2, 3, 4, 11, 41, 42],
    case iso_8583_message_utils:has_required_fields(CamposObrigatorios, Request) of
        false ->
            {error, campos_ausentes};
        true ->
            % Processar transação
            ResultCode = processar_transacao(Request),

            % Criar resposta com código
            Response = iso_8583_message_utils:create_response_with_code(
                [2, 3, 4, 11, 41, 42],  % Campos a copiar
                Request,
                ResultCode
            ),

            % Marshal e retornar
            {ok, iso_8583_marshaller:marshal(Response, binary)}
    end.

%% Tratar repeat
tratar_mensagem(Data) ->
    Msg = iso_8583_marshaller:unmarshal(Data, binary),

    case iso_8583_message_utils:is_repeat(Msg) of
        true ->
            % Buscar resposta original e reenviar
            buscar_e_reenviar_resposta_original(Msg);
        false ->
            % Processar normalmente
            processar_nova_transacao(Msg)
    end.
```

## Veja Também

- [iso_8583](../modulos/iso_8583.md) - Módulo principal de mensagens
- [iso_8583_fields](../fields/iso_8583_fields.md) - Definições de campos
- [iso_8583_marshaller](../marshallers/iso_8583_marshaller.md) - Serialização
