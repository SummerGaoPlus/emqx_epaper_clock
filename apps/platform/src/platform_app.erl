%%%-------------------------------------------------------------------
%% @doc platform public API
%% @end
%%%-------------------------------------------------------------------

-module(platform_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    platform_set_env(),
    platform_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 初始化系统默认配置
-spec platform_set_env() -> ok.
platform_set_env() ->
    %% MQTT Client
    application:set_env(platform, mqtt, [
        {clean_start, true},
        {proto_ver, v5},
        {force_ping, true},
        {keepalive, 0},
        {host, "localhost"},
        {port, 1883},
        {username, "SummerGao"},
        {password, "123456"}
    ]),
    %% Enable uart
    application:set_env(platform, uarts, [
        {uart2, [
            {enable, false},
            {open, "/dev/ttyAMA1"}
        ]}
    ]),
    %% DS18B20传感器
    application:set_env(platform, ds18b20, [
        {enable, false},
        {dir, "/sys/bus/w1/devices"},
        %% ds18b20模块
        %% {file, "/sys/bus/w1/devices/28-011939632f5b/w1_slave"}
        %% ds18b20防水型 接BCM.18引脚
        {file, "/sys/bus/w1/devices/28-3c01f095d8e4/w1_slave"}
    ]),
    %% 树莓派/香橙派 CPU
    application:set_env(platform, cpu, [
        {enable, true},
        {type, orange_pi}, %% raspberry_pi | orange_pi
        {dir, "/sys/class/thermal/"},
        {file, "/sys/class/thermal/thermal_zone0/temp"}
    ]),
    %% 第三方天气预报接口
    application:set_env(platform, weather_api, <<"http://weather.summergao.com/platform-weather/weather/api?isToken=1&cityId=">>),
    %% cron定时任务
    application:set_env(platform, crontab_jobs, [
        {summergao_clock, "@minutely", {platform_cron, summergao_clock, ["我是SummerGao的时钟，我每分钟刷新一次吖！"]}},
        {summergao_weather, "@hourly", {platform_cron, summergao_weather, [<<"101120101">>]}},
        {summergao_sensor_data_remote, "59 * * * * *", {platform_cron, summergao_sensor_data_remote, ["我是SummerGao的温湿度传感器，我每分钟的第59秒获取一次温湿度数据吖！"]}}

    ]),
    %% ecron定时任务全局配置
    application:set_env(ecron, time_zone, local),
    application:set_env(ecron, global_quorum_size, 1),
    application:set_env(ecron, global_jobs, [
%%      {global_crontab_job, "*/15 * * * * *", {platform_cron, inspect, ["Runs on 0, 15, 30, 45 seconds"]}}
    ]).
