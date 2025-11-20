-module(basic_usage).
-export([example_authorization/0, example_reversal/0, example_repeat/0]).

%% @doc Exemplo de mensagem de autorização
example_authorization() ->
    %% Criar mensagem de autorização
    Msg = iso_8583:new(),
    Msg1 = iso_8583:set_mti(<<"0200">>, Msg),
    
    %% Adicionar campos obrigatórios
    Msg2 = iso_8583:set(2, <<"4111111111111111">>, Msg1),    % PAN
    Msg3 = iso_8583:set(3, <<"000000">>, Msg2),              % Processing Code
    Msg4 = iso_8583:set(4, <<"000000001000">>, Msg3),        % Amount (R$ 10,00)
    Msg5 = iso_8583:set(11, <<"123456">>, Msg4),             % STAN
    Msg6 = iso_8583:set(41, <<"TERM0001">>, Msg5),           % Terminal ID
    Msg7 = iso_8583:set(42, <<"MERCHANT00001">>, Msg6),      % Merchant ID
    
    %% Serializar para binário
    BinaryData = iso_8583_marshaller:marshal(Msg7, binary),
    
    io:format("Mensagem de autorização criada:~n"),
    io:format("MTI: ~p~n", [iso_8583:get_mti(Msg7)]),
    io:format("Campos: ~p~n", [iso_8583:get_fields(Msg7)]),
    io:format("Dados binários (~p bytes)~n", [byte_size(BinaryData)]),
    
    {ok, Msg7, BinaryData}.

%% @doc Exemplo de criação de resposta
example_response() ->
    %% Simular recebimento de requisição
    {ok, Request, _} = example_authorization(),
    
    %% Criar resposta com código de aprovação
    Response = iso_8583_message_utils:create_response_with_code(
        [2, 3, 4, 11, 41, 42],  % Campos a copiar
        Request,
        <<"00">>                % Aprovado
    ),
    
    %% Adicionar código de aprovação
    Response1 = iso_8583:set(38, <<"123456">>, Response),
    
    io:format("~nResposta criada:~n"),
    io:format("MTI: ~p~n", [iso_8583:get_mti(Response1)]),
    io:format("Código de resposta: ~p~n", [iso_8583:get(39, Response1)]),
    io:format("Código de aprovação: ~p~n", [iso_8583:get(38, Response1)]),
    
    {ok, Response1}.

%% @doc Exemplo de mensagem de reversa
example_reversal() ->
    %% Criar mensagem de reversa
    Msg = iso_8583:new(),
    Msg1 = iso_8583:set_mti(<<"0400">>, Msg),
    
    %% Adicionar campos
    Msg2 = iso_8583:set(2, <<"4111111111111111">>, Msg1),
    Msg3 = iso_8583:set(3, <<"000000">>, Msg2),
    Msg4 = iso_8583:set(4, <<"000000001000">>, Msg3),
    Msg5 = iso_8583:set(11, <<"123456">>, Msg4),
    Msg6 = iso_8583:set(41, <<"TERM0001">>, Msg5),
    Msg7 = iso_8583:set(42, <<"MERCHANT00001">>, Msg6),
    
    %% Verificar se é reversa
    IsReversal = iso_8583_message_utils:is_reversal(Msg7),
    
    io:format("~nMensagem de reversa criada:~n"),
    io:format("MTI: ~p~n", [iso_8583:get_mti(Msg7)]),
    io:format("É reversa? ~p~n", [IsReversal]),
    
    {ok, Msg7}.

%% @doc Exemplo de mensagem repeat
example_repeat() ->
    %% Criar mensagem original
    {ok, Original, _} = example_authorization(),
    
    %% Marcar como repeat
    RepeatMsg = iso_8583_message_utils:mark_as_repeat(Original),
    
    io:format("~nMensagem repeat criada:~n"),
    io:format("MTI original: ~p~n", [iso_8583:get_mti(Original)]),
    io:format("MTI repeat: ~p~n", [iso_8583:get_mti(RepeatMsg)]),
    io:format("É repeat? ~p~n", [iso_8583_message_utils:is_repeat(RepeatMsg)]),
    
    {ok, RepeatMsg}.

%% @doc Exemplo de conversão entre formatos
example_format_conversion() ->
    {ok, Msg, _} = example_authorization(),
    
    %% Converter para diferentes formatos
    AsciiData = iso_8583_marshaller:marshal(Msg, ascii),
    BinaryData = iso_8583_marshaller:marshal(Msg, binary),
    JsonData = iso_8583_marshaller:marshal(Msg, json),
    
    io:format("~nConversão entre formatos:~n"),
    io:format("ASCII: ~p bytes~n", [byte_size(AsciiData)]),
    io:format("Binary: ~p bytes~n", [byte_size(BinaryData)]),
    io:format("JSON: ~p bytes~n", [byte_size(JsonData)]),
    
    %% Desserializar de volta
    MsgFromBinary = iso_8583_marshaller:unmarshal(BinaryData, binary),
    
    io:format("Mensagem recuperada do binário:~n"),
    io:format("MTI: ~p~n", [iso_8583:get_mti(MsgFromBinary)]),
    
    ok.
