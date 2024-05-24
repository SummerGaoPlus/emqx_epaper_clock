%%--------------------------------------------------------------------
% Copyright (c) 2024-2025 SummerGao <summergao@vip.qq.com>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%%--------------------------------------------------------------------
%% 获取 树莓派/OrangePi CPU相关信息：温度、内存
-module(platform_cpu).
-behavior(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/0]).
-export([get_temp/0]).
-export([loop_test/1]).

-record(state, {driver, enable, type}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 获取CPU/芯片温度数据
get_temp() -> gen_server:call(?MODULE, get_temp).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, CPU} = application:get_env(platform, cpu),
    case proplists:get_value(enable, CPU, false) of
        true ->
            %% 获取当前系统的类型
            case os:type() of
                {unix, linux} ->
                    Type = proplists:get_value(type, CPU),
                    case Type of
                        raspberry_pi ->
                            File = proplists:get_value(file, CPU),
                            %%判断 File 是否是一个文件或目录，如果是则返回 true，否则返回 false。
                            case filelib:is_file(File) of
                                true ->
                                    {ok, #state{driver = File, enable = true, type = Type}};
                                false ->
                                    {ok, #state{enable = false}}
                            end;
                        orange_pi ->
                            {ok, #state{enable = true, type = Type}}
                    end;
                _ ->
                    {ok, #state{enable = false}}
            end;
        false ->
            {ok, #state{enable = false}}
    end.

handle_call(get_temp, _From, #state{enable = true} = State) ->
    Result = handle_get_temp(State),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, 0.00, State}.

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(Info, _State) -> error({unknown_info, Info}).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_get_temp(#state{driver = File, type = Type}) when Type =:= raspberry_pi ->
    {ok, Original} = file:read_file(File),
    [A, _] = binary:split(Original, [<<"\n">>]),
    T = binary_to_integer(A) / 1000,
    T;
handle_get_temp(#state{type = Type}) when Type =:= orange_pi ->
    T = lists:filter(
        fun(Char) ->
            not lists:member(Char, [$\t, $\n, $ , $!] ++ [$T, $e, $p, $e, $r, $a, $t, $u, $r, $e, $m, $(, $C, $), $:])
        end,
        os:cmd("npu-smi info -t temp -i 0 -c 0")
    ),
    IsNumber = platform_utils:is_number(T),
    if
        IsNumber =:= true ->

            list_to_integer(T) * 1.0;
        true ->
            0.00
    end.


%% 调式用
-spec loop_test(atom()) -> string().
loop_test(Type) when Type =:= raspberry_pi ->
    {ok, Original} = file:read_file("/sys/class/thermal/thermal_zone0/temp"),
    [A, _] = binary:split(Original, [<<"\n">>]),
    T = binary_to_integer(A) / 1000,
    io:format("Temp ℃ : ~p~n", [T]),
    timer:sleep(1000),
    loop_test(Type);
loop_test(Type) when Type =:= orange_pi ->
    T = lists:filter(
        fun(Char) ->
            not lists:member(Char, [$\t, $\n, $ , $!] ++ [$T, $e, $p, $e, $r, $a, $t, $u, $r, $e, $m, $(, $C, $), $:])
        end,
        os:cmd("npu-smi info -t temp -i 0 -c 0")
    ),
    T2 = list_to_integer(T) * 1.0,
    io:format("Temp ℃ : ~p~n", [T2]),
    timer:sleep(1000),
    loop_test(Type).
