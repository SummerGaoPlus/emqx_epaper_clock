%%%-------------------------------------------------------------------
%% @doc platform top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(platform_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
        intensity => 0,
        period => 1},
    ChildSpecs = [
        %% MQTT Client
        {platform_client_mqtt_sup,
            {platform_client_mqtt_sup, start_link, []},
            permanent, infinity, supervisor, [platform_client_mqtt_sup]},
        %% http_client
        {platform_http_client,
            {platform_http_client, start_link, []},
            permanent, 5000, worker, [platform_http_client]},
        %% 串口通信
        {platform_serials,
            {platform_serials_sup, start_link, []},
            permanent, infinity, supervisor, [platform_serials_sup]},
        {platform_gateway,
            {platform_gateway, start_link, []},
            permanent, 5000, worker, [platform_gateway]},
        %% 墨水屏驱动
        {platform_epaper_clock,
            {platform_epaper_clock, start_link, []},
            permanent, 5000, worker, [platform_epaper_clock]},
        %% 读取CPU运行参数
        {platform_cpu,
            {platform_cpu, start_link, []},
            permanent, 5000, worker, [platform_cpu]},
        %% ModBus sensor
        {platform_modbus_sensor,
            {platform_modbus_sensor, start_link, []},
            permanent, 5000, worker, [platform_modbus_sensor]},
        %% 定时任务
        {platform_cron_sup,
            {platform_cron_sup, start_link, []},
            permanent, infinity, supervisor, [platform_cron_sup]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
