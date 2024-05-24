%%--------------------------------------------------------------------
% Copyright (c) 2024-2025 SummerGao <summergao@vip.qq.com>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%%--------------------------------------------------------------------
-module(platform_http_client).
-author("SummerGao").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([get_weather/0]).
-export([get_weather/1]).
-export([get_weather_info/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(WEATHER_TABLE, weather_table).
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
    platform_utils:ets_init(?WEATHER_TABLE),
    %% Default Get Jinan Weather
    get_weather(<<"101120101">>),
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

get_weather() ->
    Method = get,
    URL = <<"https://api.jisuapi.com/iqa/query?appkey=62958a3a6ef3c56d&question=济南天气"/utf8>>,
    Headers = [],
    Payload = <<>>,
    Options = [],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, Headers, Payload, Options),
    {ok, Body} = hackney:body(ClientRef),
    io:format("Body : ~ts~n", [Body]),
    case catch jsx:decode(Body, [return_maps, {labels, atom}]) of
        Map when is_map(Map) ->
            case Map of
                #{'result' := Result} ->
                    #{'content' := Content} = Result,
                    C = re:replace(Content, "/s\r\n", " ", [{return, list}]),
                    L = re:split(C, " ", [{return, list}]),
                    io:format("L: ~p~n", [L]),
                    io:format("L-1: ~ts~n", [lists:nth(1, L)]),
                    gen_server:call(platform_epaper_clock, {display_char, Body});
                _Else ->
                    io:format("Error: ~p~n", ["result is undefined"])
            end;
        List when is_list(List) ->
            io:format("List : ~p~n", [List]);
        _Else ->
            io:format("Error : ~p~n", ["Bad JSON Msg"])
    end.

-spec get_weather(binary()) -> ok | {error, term()}.
get_weather(CityId) when is_binary(CityId) ->
    {ok, WeatherApi} = application:get_env(platform, weather_api),
    URL = <<WeatherApi/binary, CityId/binary>>,
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{pool, default}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, Headers, Payload, Options),
    case hackney:body(ClientRef) of
        {ok, Body} ->
%%      io:format("Body : ~ts~n", [Body]),
            {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(erlang:system_time(second) + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {8, 0, 0}})),
            Ymd = dactyl:render("~year~;-~month~?0~m~;~:~m~;~;-~day~?0~d~;~:~d~;~;", [{year, Year}, {month, Month < 10}, {m, Month}, {day, Day < 10}, {d, Day}]),
            Data = maps:get(data, jsx:decode(Body, [return_maps, {labels, atom}])),
            platform_utils:ets_put(?WEATHER_TABLE, Ymd, Data);
        _Error ->
            io:format("get_weather with error: ~p~n", [_Error]),
            _Error
    end.

%% {"101120101","2021-12-10",1,weather}
get_weather_info({Ymd, N, Field}) ->
    case platform_utils:ets_get(?WEATHER_TABLE, Ymd) of
        {ok, List} ->
            Map = lists:nth(N, List),
            case Field of
                null ->
                    Map;
                _ ->
                    Value = maps:get(Field, Map),
                    io:format("Value: ~ts~n", [Value]),
                    Value
            end;
        {error, notfound} ->
            io:format("未查询到:~ts这天的天气数据！~n", [Ymd]),
            notfound
    end.
