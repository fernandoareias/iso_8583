-module(iso_8583_ebcdic_converter).

%%
%% Exported Functions
%%
-export([ascii_to_ebcdic/1, ebcdic_to_ascii/1]).

%%
%% API Functions
%%

%% @doc Converts an ASCII string to an EBCDIC string.
%%
%% @spec ascii_to_ebcdic(string()) -> list(byte())
-spec ascii_to_ebcdic(string()) -> [byte()].
ascii_to_ebcdic(Str) ->
    [ascii_to_ebcdic_char(C) || C <- Str].

%% @doc Converts an EBCDIC string to an ASCII string.
%%
%% @spec ebcdic_to_ascii(list(byte())) -> string()
-spec ebcdic_to_ascii([byte()]) -> string().
ebcdic_to_ascii(EbcdicStr) ->
    [ebcdic_to_ascii_char(C) || C <- EbcdicStr].

%%
%% Local Functions
%%

ascii_to_ebcdic_char(H) when H >= $0 andalso H =< $9 ->
    H - $0 + 240;
ascii_to_ebcdic_char(H) when H >= $a andalso H =< $i ->
    H - $a + 129;
ascii_to_ebcdic_char(H) when H >= $j andalso H =< $r ->
    H - $j + 145;
ascii_to_ebcdic_char(H) when H >= $s andalso H =< $z ->
    H - $s + 162;
ascii_to_ebcdic_char(H) when H >= $A andalso H =< $I ->
    H - $A + 193;
ascii_to_ebcdic_char(H) when H >= $J andalso H =< $R ->
    H - $J + 209;
ascii_to_ebcdic_char(H) when H >= $S andalso H =< $Z ->
    H - $S + 226;
ascii_to_ebcdic_char($ ) ->
    64;
ascii_to_ebcdic_char($.) ->
    75;
ascii_to_ebcdic_char($<) ->
    76;
ascii_to_ebcdic_char($() ->
    77;
ascii_to_ebcdic_char($+) ->
    78;
ascii_to_ebcdic_char($|) ->
    79;
ascii_to_ebcdic_char($&) ->
    80;
ascii_to_ebcdic_char($!) ->
    90;
ascii_to_ebcdic_char($$) ->
    91;
ascii_to_ebcdic_char($*) ->
    92;
ascii_to_ebcdic_char($)) ->
    93;
ascii_to_ebcdic_char($;) ->
    94;
ascii_to_ebcdic_char($-) ->
    96;
ascii_to_ebcdic_char($/) ->
    97;
ascii_to_ebcdic_char($,) ->
    107;
ascii_to_ebcdic_char($%) ->
    108;
ascii_to_ebcdic_char($_) ->
    109;
ascii_to_ebcdic_char($>) ->
    110;
ascii_to_ebcdic_char($?) ->
    111;
ascii_to_ebcdic_char($`) ->
    121;
ascii_to_ebcdic_char($:) ->
    122;
ascii_to_ebcdic_char($#) ->
    123;
ascii_to_ebcdic_char($@) ->
    124;
ascii_to_ebcdic_char($') ->
    125;
ascii_to_ebcdic_char($=) ->
    126;
ascii_to_ebcdic_char($") ->
    127;
ascii_to_ebcdic_char($~) ->
    161;
ascii_to_ebcdic_char($^) ->
    176;
ascii_to_ebcdic_char($[) ->
    186;
ascii_to_ebcdic_char($]) ->
    187;
ascii_to_ebcdic_char(${) ->
    192;
ascii_to_ebcdic_char($}) ->
    208;
ascii_to_ebcdic_char($\\) ->
    224.

ebcdic_to_ascii_char(H) when H >= 129 andalso H =< 137 ->
    H - 129 + $a;
ebcdic_to_ascii_char(H) when H >= 145 andalso H =< 153 ->
    H - 145 + $j;
ebcdic_to_ascii_char(H) when H >= 162 andalso H =< 169 ->
    H - 162 + $s;
ebcdic_to_ascii_char(H) when H >= 193 andalso H =< 201 ->
    H - 193 + $A;
ebcdic_to_ascii_char(H) when H >= 209 andalso H =< 217 ->
    H - 209 + $J;
ebcdic_to_ascii_char(H) when H >= 226 andalso H =< 233 ->
    H - 226 + $S;
ebcdic_to_ascii_char(H) when H >= 240 andalso H =< 249 ->
    H - 240 + $0;
ebcdic_to_ascii_char(64) ->
    $ ;
ebcdic_to_ascii_char(75) ->
    $.;
ebcdic_to_ascii_char(76) ->
    $<;
ebcdic_to_ascii_char(77) ->
    $(;
ebcdic_to_ascii_char(78) ->
    $+;
ebcdic_to_ascii_char(79) ->
    $|;
ebcdic_to_ascii_char(80) ->
    $&;
ebcdic_to_ascii_char(90) ->
    $!;
ebcdic_to_ascii_char(91) ->
    $$;
ebcdic_to_ascii_char(92) ->
    $*;
ebcdic_to_ascii_char(93) ->
    $);
ebcdic_to_ascii_char(94) ->
    $;;
ebcdic_to_ascii_char(96) ->
    $-;
ebcdic_to_ascii_char(97) ->
    $/;
ebcdic_to_ascii_char(107) ->
    $,;
ebcdic_to_ascii_char(108) ->
    $%;
ebcdic_to_ascii_char(109) ->
    $_;
ebcdic_to_ascii_char(110) ->
    $>;
ebcdic_to_ascii_char(111) ->
    $?;
ebcdic_to_ascii_char(121) ->
    $`;
ebcdic_to_ascii_char(122) ->
    $:;
ebcdic_to_ascii_char(123) ->
    $#;
ebcdic_to_ascii_char(124) ->
    $@;
ebcdic_to_ascii_char(125) ->
    $';
ebcdic_to_ascii_char(126) ->
    $=;
ebcdic_to_ascii_char(127) ->
    $";
ebcdic_to_ascii_char(161) ->
    $~;
ebcdic_to_ascii_char(176) ->
    $^;
ebcdic_to_ascii_char(186) ->
    $[;
ebcdic_to_ascii_char(187) ->
    $];
ebcdic_to_ascii_char(192) ->
    ${;
ebcdic_to_ascii_char(208) ->
    $};
ebcdic_to_ascii_char(224) ->
    $\\.
