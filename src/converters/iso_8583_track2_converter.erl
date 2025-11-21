-module(iso_8583_track2_converter).

%%
%% Exported Functions
%%
-export([track2_to_string/2, string_to_track2/1]).

%%
%% API Functions
%%

%% @doc Converts a list of track 2 nibbles to a string containing
%%      an ASCII encoding of the same data.
%%
%% @spec track2_to_string(list(byte()), integer()) -> string()
-spec track2_to_string([byte()], integer()) -> string().
track2_to_string(Track2Data, Length) ->
    lists:sublist(track2_to_string_impl(Track2Data, []), 1, Length).

%% @doc Converts a string of ASCII characters to a track 2 encoding.
%%
%% @spec string_to_track2(string()) -> list(byte())
-spec string_to_track2(string()) -> [byte()].
string_to_track2(Str) ->
    string_to_track2(Str, [], 0, true).

%%
%% Local Functions
%%

track2_to_string_impl([], Result) ->
    lists:reverse(Result);
track2_to_string_impl([H | Tail], Result) ->
    track2_to_string_impl(Tail, [H rem 16 + $0, H div 16 + $0 | Result]).

string_to_track2([], Result, 0, true) ->
    lists:reverse(Result);
string_to_track2([], Result, X, false) ->
    lists:reverse([X * 16 | Result]);
string_to_track2([H | T], Result, 0, true) ->
    string_to_track2(T, Result, H - $0, false);
string_to_track2([H | T], Result, X, false) ->
    Xupdated = X * 16 + H - $0,
    string_to_track2(T, [Xupdated | Result], 0, true).
