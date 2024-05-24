%%--------------------------------------------------------------------
% Copyright (c) 2024-2025 SummerGao <summergao@vip.qq.com>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%%--------------------------------------------------------------------
-module(platform_serials).
-author("SummerGao").
-behaviour(gen_server).

%% API
-export([start_link/0, open/2, send/2, close/1, restart/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {buffer}).
-define(SERIAL_TABLE, serial_table).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 打开指定串口
%% OPEN: "/dev/ttyUSB0"
%% SPEED: 115200
open(OPEN, SPEED) ->
  gen_server:call(?MODULE, {open, OPEN, SPEED}).
%% 向指定串口发送数据
send(Msg, OPEN) ->
  gen_server:call(?MODULE, {send, Msg, OPEN}).
%% 关闭指定串口
close(OPEN) ->
  gen_server:call(?MODULE, {close, OPEN}).
%% 重启指定串口
restart(OPEN, SPEED) ->
  gen_server:call(?MODULE, {restart, OPEN, SPEED}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  platform_utils:ets_init(?SERIAL_TABLE),
  State = #state{buffer = []},
  {ok, State}.

%% 打开串口
handle_call({open, OPEN, SPEED}, _From, _State) ->
  %% 设置USB端口号及串口波特率
  SerialPort = serial:start([{open, OPEN}, {speed, SPEED}]),
  platform_utils:ets_put(?SERIAL_TABLE, OPEN, SerialPort),
  State = #state{buffer = []},
  {reply, ok, State};
%% 处理发送数据请求
handle_call({send, Msg, OPEN}, _From, State) ->
  case platform_utils:ets_get(?SERIAL_TABLE, OPEN) of
    {ok, SerialPort} ->
%%      SerialPort ! {send, Msg ++ "\r\n"},
      SerialPort ! {send, Msg},
      {reply, ok, State};
    _ ->
      {reply, {error, unknown_serial_port}, State}
  end;
%% 关闭串口
handle_call({close, OPEN}, _From, State) ->
  case platform_utils:ets_get(?SERIAL_TABLE, OPEN) of
    {ok, SerialPort} ->
      SerialPort ! {close},
      platform_utils:ets_delete(?SERIAL_TABLE, OPEN),
      {reply, ok, State};
    _ ->
      {reply, {error, unknown_serial_port}, State}
  end;
%% 重启串口
handle_call({restart, OPEN, SPEED}, _From, _State) ->
  SerialPort = serial:start([{open, OPEN}, {speed, SPEED}]),
  platform_utils:ets_put(?SERIAL_TABLE, OPEN, SerialPort),
  State = #state{buffer = []},
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

%% 接收串口数据
handle_info({data, Bytes}, State = #state{buffer = Buffer}) ->
  Data = binary_to_list(Bytes),
  R = string:str(Data, "\n"),
  if
    R > 0 ->
      NewBuffer = Buffer ++ Data,
      L = string:tokens(NewBuffer, "\r\n"),
      %%  获取列表大小
      X = iolist_size(L),
      if X =:= 0 ->
        NewState = State#state{buffer = []},
        {noreply, NewState};
        true ->
          [H | T] = L,
          io:format("Serial Data : ~ts~n", [H]),
          NewState = State#state{buffer = lists:concat(T)},
          {noreply, NewState}
      end;
    true ->
      NewBuffer = Buffer ++ Data,
      NewState = State#state{buffer = NewBuffer},
      {noreply, NewState}
  end;
handle_info(_Info, State) ->
  io:format("unknown info : ~p~n",[_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("terminate : ~p~n", [_Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
