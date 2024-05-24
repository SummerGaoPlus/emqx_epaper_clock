%%--------------------------------------------------------------------
% Copyright (c) 2024-2025 SummerGao <summergao@vip.qq.com>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%%--------------------------------------------------------------------
-module(platform_modbus_sensor).
-author("SummerGao").

-behaviour(gen_server).

-include_lib("emqx/include/logger.hrl").

%% API
-export([start_link/0]).
-export([handle_msg/2]).
-export([get_sensor_data_remote/0, get_sensor_data/1, get_temp/0, get_hum/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MODBUS_SENSOR_TABLE, modbus_sensor_table).
-define(DEVICE_ID, "\x01\x03\x00\x00\x00\x02\xC4\x0B").
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    platform_utils:ets_init(?MODBUS_SENSOR_TABLE),
    %% 订阅传感器
    platform_client_mqtt:subscribe_topic(<<"wifi_dtu_topic_pub">>),
    %% 请求传感器数据
    get_sensor_data_remote(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({wifi_dtu_topic_pub, Msg}, State) ->
    List = binary_to_list(Msg),
    case platform_modbus_rtu_crc:check_crc(List) of
        true ->
            [_, _, _, TH, TL, HH, HL, _, _] = [integer_to_list(X, 16) || X <- List],
            Temp = list_to_integer(TH ++ TL, 16),
            Hum = list_to_integer(HH ++ HL, 16),
            %% 这里写入的是矫正后的温湿度传感器数据
            platform_utils:ets_put(?MODBUS_SENSOR_TABLE, ?DEVICE_ID, #{temp => Temp / 100 + 1, hum => Hum / 100 - 6});
        false ->
            ?SLOG(error, #{
                msg => "Abnormal temperature and humidity sensor data",
                reason => List
            }),
            platform_utils:ets_put(?MODBUS_SENSOR_TABLE, ?DEVICE_ID, #{temp => 0.00, hum => 0.00})
    end,

    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_msg(binary(), iodata()) -> ok.
handle_msg(Topic, Msg) ->
    gen_server:cast(?MODULE, {Topic, Msg}).

%% 获取温度
get_temp() ->
    case get_sensor_data(?DEVICE_ID) of
        #{temp := Temp} ->
            Temp;
        notfound ->
            0.00
    end.
%% 获取湿度
get_hum() ->
    case get_sensor_data(?DEVICE_ID) of
        #{hum := Hum} ->
            Hum;
        notfound ->
            0.00
    end.

get_sensor_data(DeviceId) ->
    case platform_utils:ets_get(?MODBUS_SENSOR_TABLE, DeviceId) of
        {ok, Data} ->
            Data;
        {error, notfound} ->
            ?SLOG(error, #{
                msg => <<"未查询到:"/utf8, (list_to_binary(DeviceId))/binary, "传感器的数据"/utf8>>
            }),
            notfound
    end.

%% 请求传感器数据
get_sensor_data_remote() ->
    platform_client_mqtt:publish_msg(<<"wifi_dtu_topic_sub">>, ?DEVICE_ID).
