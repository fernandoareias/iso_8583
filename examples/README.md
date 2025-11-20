# ISO 8583 - Exemplos de Uso

Este diretório contém exemplos práticos de como usar a biblioteca ISO 8583.

## Executando os Exemplos

### No shell do rebar3:

```bash
make shell
```

Ou:

```bash
rebar3 shell
```

### Executar exemplos:

```erlang
%% Compilar o módulo de exemplos
c("examples/basic_usage.erl").

%% Exemplo de autorização
basic_usage:example_authorization().

%% Exemplo de reversa
basic_usage:example_reversal().

%% Exemplo de repeat
basic_usage:example_repeat().

%% Exemplo de conversão de formatos
basic_usage:example_format_conversion().
```

## Exemplos Disponíveis

### 1. Autorização Básica (`example_authorization/0`)

Demonstra como criar uma mensagem de autorização (MTI 0200) com os campos básicos:
- PAN (Primary Account Number)
- Processing Code
- Amount
- STAN (Systems Trace Audit Number)
- Terminal ID
- Merchant ID

### 2. Criação de Resposta (`example_response/0`)

Mostra como criar uma resposta automática a partir de uma requisição, copiando campos relevantes e adicionando código de resposta.

### 3. Reversa (`example_reversal/0`)

Exemplo de como criar uma mensagem de reversa (MTI 0400) e verificar se a mensagem é uma reversa.

### 4. Repeat (`example_repeat/0`)

Demonstra como marcar uma mensagem como repeat e verificar a flag de repeat.

### 5. Conversão de Formatos (`example_format_conversion/0`)

Mostra como converter uma mensagem entre diferentes formatos (ASCII, Binary, JSON) e como desserializar de volta.

## Casos de Uso Comuns

### Processar Transação Completa

```erlang
processar_transacao(Data) ->
    %% Unmarshal
    Request = iso_8583_marshaller:unmarshal(Data, binary),
    
    %% Validar
    ok = iso_8583_message_utils:validate_mti(Request),
    true = iso_8583_message_utils:is_request(Request),
    
    %% Processar (implementar sua lógica aqui)
    ResultCode = case processar_autorizacao(Request) of
        approved -> <<"00">>;
        insufficient_funds -> <<"51">>;
        _ -> <<"05">>
    end,
    
    %% Criar resposta
    Response = iso_8583_message_utils:create_response_with_code(
        [2, 3, 4, 11, 41, 42],
        Request,
        ResultCode
    ),
    
    %% Marshal e retornar
    iso_8583_marshaller:marshal(Response, binary).
```

### Validar Campos Obrigatórios

```erlang
validar_requisicao(Request) ->
    CamposObrigatorios = [2, 3, 4, 11, 41, 42],
    case iso_8583_message_utils:has_required_fields(CamposObrigatorios, Request) of
        true -> ok;
        false -> {error, campos_ausentes}
    end.
```

### Trabalhar com Diferentes Versões ISO

```erlang
processar_por_versao(Msg) ->
    Version = iso_8583_message_utils:get_mti_version(Msg),
    case Version of
        '1987' -> processar_1987(Msg);
        '1993' -> processar_1993(Msg);
        '2003' -> processar_2003(Msg);
        _ -> {error, versao_desconhecida}
    end.
```

## Documentação Completa

Para documentação completa, consulte:
- [Documentação em Português](../docs/README.md)
- [English Documentation](../docs/en/README.md)
