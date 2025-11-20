-module(iso_8583_bitmap_converter).

%%
%% Exported Functions
%%
-export([
    list_to_bitmap/2,
    bitmap_to_list/2
]).

%%
%% API Functions
%%

%% @doc Converts a list of integer IDs to a 64-bit bitmap.
%%      Values in the range [Offset+1, Offset+64] are
%%      encoded. Values outside the specified range are
%%      ignored.
%%
%% @spec list_to_bitmap(list(integer()), integer()) -> binary()
-spec(list_to_bitmap(list(integer()), integer()) -> binary()).

list_to_bitmap(FieldIds, Offset) ->
    list_to_bitmap(FieldIds, Offset, array:from_list(lists:duplicate(8, 0))).

%% @doc Converts a 64-bit bitmap to a list of integers
%%      starting at a specified offset.
%%
%% @spec bitmap_to_list(binary(), integer()) -> list(integer())
-spec(bitmap_to_list(binary(), integer()) -> list(integer())).

bitmap_to_list(Bitmap, Offset) when size(Bitmap) =:= 8 ->
    <<BitmapInt:64/big>> = Bitmap,
    bitmap_int_to_list(BitmapInt, Offset, 0, []).

%%
%% Local Functions
%%

list_to_bitmap([], _Offset, Result) ->
    list_to_binary(array:to_list(Result));
list_to_bitmap([Id | Tail], Offset, Result) when Id > Offset andalso Id =< Offset + 64 ->
    Id2 = Id - Offset - 1,
    Index = Id2 div 8,
    BitNum = 7 - (Id2 rem 8),
    CurValue = array:get(Index, Result),
    NewValue = CurValue bor (1 bsl BitNum),
    list_to_bitmap(Tail, Offset, array:set(Index, NewValue, Result));
list_to_bitmap([_Id | Tail], Offset, Result) ->
    list_to_bitmap(Tail, Offset, Result).

bitmap_int_to_list(_Value, Offset, 64, Result) ->
    [Index + Offset || Index <- Result];
bitmap_int_to_list(Value, Offset, N, Result) ->
    case Value band (1 bsl N) of
        0 ->
            bitmap_int_to_list(Value, Offset, N + 1, Result);
        _ ->
            bitmap_int_to_list(Value, Offset, N + 1, [64 - N | Result])
    end.
