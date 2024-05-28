%%--------------------------------------------------------------------
% Copyright (c) 2024-2025 SummerGao <summergao@vip.qq.com>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%%--------------------------------------------------------------------
-module(platform_client_mqtt).
-author("SummerGao").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([
    publish_msg/2,
    publish_msg/3,
    subscribe_topic/1,
    subscribe_topic/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {conn}).

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
    Options1 = [{clientid, integer_to_binary(erlang:system_time(nanosecond))}, {owner, self()}],
    {ok, Options2} = application:get_env(platform, mqtt),
    {ok, ConnPid} = emqtt:start_link(Options1 ++ Options2),
    {ok, _Props} = emqtt:connect(ConnPid),
    {ok, #state{conn = ConnPid}}.

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
handle_call({publish, Topic, Msg}, _From, #state{conn = ConnPid} = State) ->
    ok = emqtt:publish(ConnPid, Topic, #{}, Msg, [{qos, 0}]),
    {reply, ok, State};
handle_call({subscribe, Topic}, _From, #state{conn = ConnPid} = State) ->
    {ok, _Props, _ReasonCodes} = emqtt:subscribe(ConnPid, {Topic, 0}),
    {reply, ok, State};
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
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% {
%% publish,
%% #{client_pid => <0.1080.0>,
%% dup => false,packet_id => undefined,
%% payload => <<"{\n  \"msg\": \"hello\"\n}">>,
%% properties => #{},
%% qos => 0,
%% retain => false,
%% topic => <<"test">>}
%% }
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({publish, #{'payload' := Msg, 'topic' := <<"serial_epaper">>}}, State) ->
    %% 消息转发
    platform_epaper_clock:handle_msg(Msg),
    {noreply, State};
handle_info({publish, #{'payload' := Msg, 'topic' := <<"wifi_dtu_topic_pub">>}}, State) ->
    %% 消息转发
    platform_modbus_sensor:handle_msg(wifi_dtu_topic_pub, Msg),
    {noreply, State};
handle_info({publish, #{'payload' := Msg}}, State) ->
    io:format("Other Msg : ~p~n",[Msg]),
    {noreply, State};
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
terminate(Reason, #state{conn = _ConnPid}) when Reason == normal; Reason == shutdown ->
    ok;
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Publish a message to a specified topic.
%%
%% @spec publish_msg(binary(), iodata()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec publish_msg(binary(), iodata()) -> ok.
publish_msg(Topic, Msg) ->
    gen_server:call(?MODULE, {publish, Topic, Msg}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Publish a message to a specified topic.
%%
%% @spec publish_msg(binary(), iodata(), pid()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec publish_msg(binary(), iodata(), pid()) -> ok.
publish_msg(Topic, Msg, ConnPid) ->
    ok = emqtt:publish(ConnPid, Topic, #{}, Msg, [{qos, 0}]).

%%--------------------------------------------------------------------
%% @doc
%% Subscribe to messages.
%%
%% @spec subscribe_topic(binary()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec subscribe_topic(binary()) -> ok.
subscribe_topic(Topic) ->
    gen_server:call(?MODULE, {subscribe, Topic}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Subscribe to messages.
%%
%% @spec subscribe_topic(binary(), pid()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec subscribe_topic(binary(), pid()) -> ok.
subscribe_topic(Topic, ConnPid) ->
    emqtt:subscribe(ConnPid, {Topic, 0}),
    ok.
