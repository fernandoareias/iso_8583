-module(new_api_usage).
-export([run/0]).

-include("iso_8583_fields_id.hrl").

%% Exemplo de uso da NOVA API simplificada do iso_8583

run() ->
    io:format("~n=== NOVA API - encode/decode ===~n~n"),

    %% 1. Criar e codificar mensagem usando map (forma mais simples)
    io:format("1. Codificar mensagem de autorização:~n"),
    MessageData = #{
        0 => <<"0200">>,              % MTI
        2 => <<"4111111111111111">>,  % PAN
        3 => <<"000000">>,            % Processing Code
        4 => <<"000000001000">>,      % Amount (10.00)
        11 => <<"123456">>            % STAN
    },

    Binary = iso_8583:encode(MessageData),
    io:format("   Mensagem codificada: ~p bytes~n", [byte_size(Binary)]),

    %% 2. Decodificar mensagem
    io:format("~n2. Decodificar mensagem:~n"),
    Decoded = iso_8583:decode(Binary),
    io:format("   MTI: ~s~n", [maps:get(0, Decoded)]),
    io:format("   PAN: ~s~n", [maps:get(2, Decoded)]),
    io:format("   Amount: ~s~n", [maps:get(4, Decoded)]),

    %% 3. Usando macros para maior legibilidade
    io:format("~n3. Usando macros de campos:~n"),
    MessageWithMacros = #{
        ?MTI => <<"0200">>,
        ?PAN => <<"4111111111111111">>,
        ?PROC_CODE => <<"000000">>,
        ?AMOUNT_TRAN => <<"000000005000">>,
        ?SYSTEMS_TRACE_AUDIT_NUMBER => <<"654321">>,
        ?CARD_ACCEPTOR_TERMINAL_ID => <<"TERM0001">>,
        ?CARD_ACCEPTOR_ID_CODE => <<"MERCHANT001">>
    },

    Binary2 = iso_8583:encode(MessageWithMacros),
    io:format("   Mensagem codificada: ~p bytes~n", [byte_size(Binary2)]),

    %% 4. Codificar em diferentes formatos
    io:format("~n4. Diferentes formatos:~n"),

    % JSON
    JsonBinary = iso_8583:encode(MessageData, #{format => json}),
    io:format("   JSON (~p bytes):~n   ~s~n", [byte_size(JsonBinary), JsonBinary]),

    % ASCII
    AsciiBinary = iso_8583:encode(MessageData, #{format => ascii}),
    io:format("   ASCII (~p bytes)~n", [byte_size(AsciiBinary)]),

    %% 5. Opções de decodificação
    io:format("~n5. Opções de decodificação:~n"),

    % Retornar como proplist
    Proplist = iso_8583:decode(Binary, #{output => proplist}),
    io:format("   Proplist: ~p~n", [Proplist]),

    % Retornar como record (API antiga)
    Record = iso_8583:decode(Binary, #{output => record}),
    io:format("   Record fields: ~p~n", [iso_8583:get_fields(Record)]),

    %% 6. Roundtrip test
    io:format("~n6. Teste de roundtrip:~n"),
    Original = #{0 => <<"0200">>, 2 => <<"4111111111111111">>, 11 => <<"999999">>},
    Encoded = iso_8583:encode(Original),
    Roundtrip = iso_8583:decode(Encoded),

    case Original =:= Roundtrip of
        true -> io:format("   ✓ Roundtrip OK!~n");
        false -> io:format("   ✗ Roundtrip FAILED!~n")
    end,

    %% 7. Comparação API antiga vs nova
    io:format("~n=== COMPARAÇÃO ===~n"),
    io:format("~nAPI ANTIGA (verbosa, 2 módulos):~n"),
    io:format("  Msg = iso_8583:new(),~n"),
    io:format("  Msg1 = iso_8583:set_mti(<<\"0200\">>, Msg),~n"),
    io:format("  Msg2 = iso_8583:set(2, <<\"4111111111111111\">>, Msg1),~n"),
    io:format("  Msg3 = iso_8583:set(3, <<\"000000\">>, Msg2),~n"),
    io:format("  Msg4 = iso_8583:set(4, <<\"000000001000\">>, Msg3),~n"),
    io:format("  Binary = iso_8583_marshaller:marshal(Msg4, binary).~n"),

    io:format("~nAPI NOVA (concisa, 1 módulo):~n"),
    io:format("  Binary = iso_8583:encode(#{~n"),
    io:format("      0 => <<\"0200\">>,~n"),
    io:format("      2 => <<\"4111111111111111\">>,~n"),
    io:format("      3 => <<\"000000\">>,~n"),
    io:format("      4 => <<\"000000001000\">>~n"),
    io:format("  }).~n"),

    io:format("~n✓ Todos os exemplos executados com sucesso!~n~n"),
    ok.
