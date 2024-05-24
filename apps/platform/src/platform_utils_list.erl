%%--------------------------------------------------------------------
% Copyright (c) 2024-2025 SummerGao <summergao@vip.qq.com>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%%--------------------------------------------------------------------
-module(platform_utils_list).
-author("SummerGao").

%% API
-export([property_deep_get/2]).
-export([property_deep_get/3]).

-type config_key() :: atom() | binary() | [byte()].
-type config_key_path() :: [config_key()].
-define(CONFIG_NOT_FOUND_MAGIC, '$0tFound').


-spec property_deep_get(config_key_path(), list()) -> term().
property_deep_get(ConfKeyPath, List) ->
    case property_deep_get(ConfKeyPath, List, ?CONFIG_NOT_FOUND_MAGIC) of
        ?CONFIG_NOT_FOUND_MAGIC -> error({config_not_found, ConfKeyPath});
        Res -> Res
    end.

-spec property_deep_get(config_key_path(), list(), term()) -> term().
property_deep_get(ConfKeyPath, List, Default) ->
    case property_deep_find(ConfKeyPath, List) of
        {not_found, _KeyPath, _Data} -> Default;
        {ok, Data} -> Data
    end.


-spec property_deep_find(config_key_path(), list()) -> {ok, term()} | {not_found, config_key_path(), term()}.
property_deep_find([], List) ->
    {ok, List};
property_deep_find([Key | KeyPath] = Path, List) when is_list(List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, SubList} -> property_deep_find(KeyPath, SubList);
        false -> {not_found, Path, List}
    end;
property_deep_find(KeyPath, Data) ->
    {not_found, KeyPath, Data}.
