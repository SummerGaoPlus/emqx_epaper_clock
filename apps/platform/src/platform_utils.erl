%%--------------------------------------------------------------------
% Copyright (c) 2024-2025 SummerGao <summergao@vip.qq.com>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%%--------------------------------------------------------------------
-module(platform_utils).

-export([binary_to_hex/1, hex_to_binary/1, reverse/1]).
-export([index_of/2]).
-export([precise_universal_time/0, time_to_gps/0, time_to_gps/1, time_to_unix/0, time_to_unix/1]).
-export([ms_diff/2, datetime_to_timestamp/1, apply_offset/2]).
-export([binary_to_hex_str/1, hex_str_to_binary/1]).
-export([strip_chars/2, is_number/1]).
-export([ets_init/1, ets_get/2, ets_put/3, ets_delete/2]).


-define(MEGA, 1000000).

binary_to_hex(undefined) ->
    undefined;
binary_to_hex(Id) ->
    <<<<Y>> || <<X:4>> <= Id, Y <- integer_to_list(X, 16)>>.

hex_to_binary(undefined) ->
    undefined;
hex_to_binary(Id) ->
    <<<<Z>> || <<X:8, Y:8>> <= Id, Z <- [binary_to_integer(<<X, Y>>, 16)]>>.

%% 二进制转16进制字符串
binary_to_hex_str(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) ||
        X <- binary_to_list(Bin)]).

%% 16进制字符串转二进制
hex_str_to_binary(S) ->
    hex_str_to_binary(S, []).
hex_str_to_binary([], Acc) ->
    list_to_binary(lists:reverse(Acc));
hex_str_to_binary([X, Y | T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [X, Y]),
    hex_str_to_binary(T, [V | Acc]);
hex_str_to_binary([X | T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", lists:flatten([X, "0"])),
    hex_str_to_binary(T, [V | Acc]).

reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<>>, Acc) -> Acc;
reverse(<<H:1/binary, Rest/binary>>, Acc) ->
    reverse(Rest, <<H/binary, Acc/binary>>).


index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _) -> undefined;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | Tl], Index) -> index_of(Item, Tl, Index + 1).


ms_diff({MSecs1, Secs1, USecs1}, {MSecs2, Secs2, USecs2}) when MSecs1 =< MSecs2 ->
    1000 * (?MEGA * (MSecs2 - MSecs1) + (Secs2 - Secs1))
        + (USecs2 - USecs1) div 1000.

precise_universal_time() ->
    {Date, {Hours, Min, Secs}} = calendar:universal_time(),
    {_, _, USecs} = erlang:timestamp(),
    {Date, {Hours, Min, Secs + (USecs div 1000) / 1000}}.

time_to_gps() ->
    time_to_gps(precise_universal_time()).

time_to_gps({Date, {Hours, Min, Secs}}) ->
    TotalSecs = calendar:datetime_to_gregorian_seconds({Date, {Hours, Min, trunc(Secs)}})
        - calendar:datetime_to_gregorian_seconds({{1980, 1, 6}, {0, 0, 0}})
        + 17, % leap seconds
    trunc(1000 * (TotalSecs + (Secs - trunc(Secs)))). % ms

time_to_unix() ->
    time_to_gps(precise_universal_time()).

time_to_unix({Date, {Hours, Min, Secs}}) ->
    TotalSecs = calendar:datetime_to_gregorian_seconds({Date, {Hours, Min, trunc(Secs)}})
        - epoch_seconds(),
    trunc(1000 * (TotalSecs + (Secs - trunc(Secs)))). % ms

datetime_to_timestamp({Date, {Hours, Min, Secs}}) ->
    TotalSecs =
        calendar:datetime_to_gregorian_seconds({Date, {Hours, Min, trunc(Secs)}})
            - epoch_seconds(),
    {TotalSecs div ?MEGA, TotalSecs rem ?MEGA, trunc(?MEGA * Secs) - ?MEGA * trunc(Secs)};
datetime_to_timestamp(undefined) ->
    {0, 0, 0}. %% midnight

epoch_seconds() ->
    calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).

apply_offset({Date, {Hours, Min, Secs}}, {OHours, OMin, OSecs}) ->
    TotalSecs =
        calendar:datetime_to_gregorian_seconds({Date, {Hours, Min, trunc(Secs)}})
            + (60 * ((60 * OHours) + OMin)) + OSecs,
    {Date2, {Hours2, Min2, Secs2}} = calendar:gregorian_seconds_to_datetime(TotalSecs),
    {Date2, {Hours2, Min2, Secs2 + (Secs - trunc(Secs))}}.

%% 去除字符串中的指定字符
%% 输入：Str = "Hello\t \nWorld!\n ",
%% ExcludeChars = [$\t, $\n, $ , $!], % 要排除的字符列表
%% 输出："HelloWorld"
strip_chars(Str, ExcludeChars) ->
    lists:filter(
        fun(Char) ->
            not lists:member(Char, ExcludeChars)
        end, Str
    ).

%% 判断是否是数字符串
is_number(String) ->
    case re:run(String, "^-?[0-9]+$", [{capture, none}]) of
        match -> true;
        nomatch -> false
    end.

%%%===================================================================
%%% ETS表操作
%%%===================================================================

%% 创建
ets_init(TABLE) ->
    case ets:info(TABLE, size) of
        undefined ->
            ets:new(TABLE, [public, named_table]);
        _ ->
            ok
    end.
%% 查询
ets_get(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            {error, notfound};
        [{_Key, Value}] ->
            {ok, Value}
    end.
%% 插入
ets_put(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}),
    ok.
%% 删除
ets_delete(Table, Key) ->
    ets:delete(Table, Key).

% end of file
