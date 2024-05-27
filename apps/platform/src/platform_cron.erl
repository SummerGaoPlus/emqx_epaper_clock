%%--------------------------------------------------------------------
% Copyright (c) 2024-2025 SummerGao <summergao@vip.qq.com>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%%--------------------------------------------------------------------
-module(platform_cron).

%% 设置定时任务
%% API
-export([delete_job/1]).
-export([job_stats/1, all_job_stats/0, predict_datetime_by_spec/2]).
-export([deactivate_job/1, activate_job/1]).
-export([deactivate_job/2, activate_job/2]).


-export([summergao_clock/0, summergao_clock/1]).
-export([summergao_weather/1]).
-export([summergao_ds18b20/1]).
-export([summergao_sensor_data_remote/1]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 时钟
-spec summergao_clock(list()) -> ok.
summergao_clock(_Format) ->
    %% 唤醒
    platform_epaper_clock:wake_up(1),
    summergao_clock(),
    timer:sleep(10),
    %% 休眠
    platform_epaper_clock:send_command(system_sleep),
    platform_epaper_clock:wake_up(0).
summergao_clock() ->
    X = 10,
    Y = 10,
    L = ["NUM0.BMP", "NUM1.BMP", "NUM2.BMP", "NUM3.BMP", "NUM4.BMP", "NUM5.BMP", "NUM6.BMP", "NUM7.BMP", "NUM8.BMP", "NUM9.BMP"],
    WeekList = ["星期一", "星期二", "星期三", "星期四", "星期五", "星期六", "星期日"],
    {{Year, Month, Day}, {Hour, Minute, _Second}} = calendar:now_to_local_time(os:timestamp()),
    io:format("Time : ~p~n", [{{Year, Month, Day}, {Hour, Minute, _Second}}]),
    platform_epaper_clock:send_command(clear_screen),
    %%  显示时间
    platform_epaper_clock:send_command({display_image, {X, Y}, lists:nth((Hour div 10) + 1, L)}),
    platform_epaper_clock:send_command({display_image, {X + 100, Y}, lists:nth((Hour rem 10) + 1, L)}),
    platform_epaper_clock:send_command({display_image, {X + 200, Y}, "NUMS.BMP"}),
    platform_epaper_clock:send_command({display_image, {X + 265, Y}, lists:nth((Minute div 10) + 1, L)}),
    platform_epaper_clock:send_command({display_image, {X + 360, Y}, lists:nth((Minute rem 10) + 1, L)}),
    %% 设置字号
    platform_epaper_clock:send_command(set_china_font_size_48),
    %% 设置字号
    platform_epaper_clock:send_command(set_english_font_size_48),
    %% 显示年月日
    Ymd = dactyl:render("~year~;-~month~?0~m~;~:~m~;~;-~day~?0~d~;~:~d~;~;", [{year, Year}, {month, Month < 10}, {m, Month}, {day, Day < 10}, {d, Day}]),
    platform_epaper_clock:send_command({display_char, {X + 500, Y + 20}, Ymd}),
    %% 显示星期几
    DayWeek = calendar:day_of_the_week({Year, Month, Day}),
    platform_epaper_clock:send_command({display_char, {X + 550, Y + 80}, lists:nth(DayWeek, WeekList)}),
    %% 绘制分割线
    platform_epaper_clock:send_command({draw_base_straight_line, {0, 160, 800, 160}}),
    %%  显示功夫熊猫
    %% platform_epaper_clock:send_command( {display_image, {200, 200}, "PIC3.BMP"}),
    %% 获取天气数据
    WeatherMap = #{
        <<"晴"/utf8>> => "WQING.BMP", <<"阴"/utf8>> => "WYIN.BMP", <<"多云转晴"/utf8>> => "WDYZQ.BMP", <<"多云"/utf8>> => "WDYZQ.BMP",
        <<"雷阵雨"/utf8>> => "WLZYU.BMP", <<"小雨"/utf8>> => "WXYU.BMP", <<"中雨"/utf8>> => "WYU.BMP",
        <<"冰雹"/utf8>> => "WBBAO.BMP", <<"雾"/utf8>> => "WWU.BMP", <<"雪"/utf8>> => "WXUE.BMP", <<"雨夹雪"/utf8>> => "WXUE.BMP", "PIC3.BMP" => "PIC3.BMP"
    },
    WeatherInfo = platform_http_client:get_weather_info({Ymd, 1, null}),
    Weather0 = maps:get(weather0, WeatherInfo),
    WeatherImage0 = maps:get(Weather0, WeatherMap, undefined),
    Weather1 = maps:get(weather1, WeatherInfo),
    WeatherImage1 = maps:get(Weather1, WeatherMap, undefined),
    if
        (WeatherImage0 =/= undefined) and (WeatherImage1 =/= undefined) ->
            platform_epaper_clock:send_command({display_image, {5, 180}, WeatherImage0}),
            if
                WeatherImage0 =:= WeatherImage1 ->
                    ok;
                true ->
                    platform_epaper_clock:send_command({display_image, {5, 350}, WeatherImage1})
            end;
        true ->
            true
    end,
    %% 设置字号
    platform_epaper_clock:send_command(set_china_font_size_32),
    %%  显示城市名
    platform_epaper_clock:send_command({display_char, {200, 190}, "济南"}),
    %% 设置字号
    platform_epaper_clock:send_command(set_china_font_size_64),
    %% "晴转多云"
    platform_epaper_clock:send_command({display_char, {300, 180}, maps:get(weather, WeatherInfo)}),
    %% 设置字号
    platform_epaper_clock:send_command(set_china_font_size_32),
    platform_epaper_clock:send_command(set_english_font_size_32),
    %% 显示温度
    platform_epaper_clock:send_command({display_char, {300, 260}, maps:get(temp, WeatherInfo)}),
    %% 显示风向
    platform_epaper_clock:send_command({display_char, {420, 260}, maps:get(windDirection, WeatherInfo)}),
    %% 显示风速
    platform_epaper_clock:send_command({display_char, {300, 300}, maps:get(windSpeed, WeatherInfo)}),

    %% 室内
    platform_epaper_clock:send_command({display_char, {200, 350}, "室内"}),
    %% 设置字号
    platform_epaper_clock:send_command(set_english_font_size_48),
    %% 显示室内温度
%%  T = platform_ds18b20:get_temp(),
%%  TS = io_lib:format("~.2f℃", [T]),
%% platform_epaper_clock:send_command( {display_char, {300, 340}, TS}),
    %% 显示室内温湿度
    if
        Minute rem 2 =:= 0 ->
            T = platform_modbus_sensor:get_temp(),
            TS = io_lib:format("~.2f℃", [T]),
            platform_epaper_clock:send_command({display_char, {300, 340}, TS});
        true ->
            H = platform_modbus_sensor:get_hum(),
            HS = io_lib:format("~.2f%", [H]),
            platform_epaper_clock:send_command({display_char, {300, 340}, HS})
    end,

    %% 设置字号
    platform_epaper_clock:send_command(set_china_font_size_32),
    %% 设置字号
    platform_epaper_clock:send_command(set_english_font_size_32),
    %% CPU
    platform_epaper_clock:send_command({display_char, {200, 420}, "CPU"}),
    %% 设置字号
    platform_epaper_clock:send_command(set_english_font_size_48),
    %% 显示CPU温度
    CT = platform_cpu:get_temp(),
    CTS = io_lib:format("~.2f℃", [CT]),
    platform_epaper_clock:send_command({display_char, {300, 410}, CTS}),

    %% 设置字号
    platform_epaper_clock:send_command(set_china_font_size_32),
    platform_epaper_clock:send_command(set_english_font_size_32),
    %% 天气预告
    platform_epaper_clock:send_command({display_char, {600, 315}, "天气预告"}),
    WeatherInfoNotice = platform_http_client:get_weather_info({Ymd, (Minute rem 10 rem 6) + 1, null}),
    platform_epaper_clock:send_command({display_char, {558, 370}, maps:get(week, WeatherInfoNotice)}),
    platform_epaper_clock:send_command({display_char, {628, 370}, maps:get(weather, WeatherInfoNotice)}),
    platform_epaper_clock:send_command({display_char, {558, 420}, maps:get(temp, WeatherInfoNotice)}),
    platform_epaper_clock:send_command({display_char, {558, 470}, maps:get(windDirection, WeatherInfoNotice)}),
    platform_epaper_clock:send_command({display_char, {558, 520}, maps:get(windSpeed, WeatherInfoNotice)}),

    %% 绘制分割线
    platform_epaper_clock:send_command({draw_base_straight_line, {550, 360, 795, 360}}),
    %% 绘制矩形
    platform_epaper_clock:send_command({draw_base_rectangle, {550, 300, 795, 580}}),
    %% 设置字号
    platform_epaper_clock:send_command(set_china_font_size_48),
    %% 底部信息栏
    BottomList = ["OrangePi AIpro","www.summergao.com", "精益求精", "勤学敏思笃行", "相信明天会更好", "物联网智能传感器", "summergao.blog.csdn.net", "香橙派 AIpro"],
    platform_epaper_clock:send_command({display_char, {20, 538}, lists:nth((Minute rem 10 rem 6) + 1, BottomList)}),

    platform_epaper_clock:send_command(update).

%% 定时获取天气信息
summergao_weather(CityId) ->
    platform_http_client:get_weather(CityId).

%% 定时获取温湿度传感数据
summergao_sensor_data_remote(_Format) ->
    platform_modbus_sensor:get_sensor_data_remote().

%% ds18b20传感器
summergao_ds18b20(Format) ->
    T = platform_ds18b20:get_temp(),
    F = io_lib:format("办公室当前温度：~.2f℃", [T]),
    U = unicode:characters_to_binary(F, utf8),
    M = iconv:convert(<<"utf-8">>, <<"utf-8">>, U),
    S = float_to_list(T, [{decimals, 2}]),
    io:format(calendar:system_time_to_rfc3339(erlang:system_time(second)) ++ " : " ++ Format ++ "办公室当前温度：" ++ S ++ "\n"),
    gen_server:call(platform_client_mqtt, {publish, <<"topic/sysz_sensor/ds18b20">>, M}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Manage Functions Begin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Delete a specific task
delete_job(JobName) ->
    ok = ecron:delete(JobName).

%% Pause job temporary
deactivate_job(JobName) ->
    ecron:deactivate(JobName).

%% Pause job temporary
%% platform_cron:deactivate_job(platform_cron,summergao_clock).
deactivate_job(Register, JobName) ->
    ecron:deactivate(Register, JobName).

%% Rerun job.
activate_job(JobName) ->
    ecron:activate(JobName).

%% Rerun job.
%% platform_cron:activate_job(platform_cron,summergao_clock).
activate_job(Register, JobName) ->
    ecron:activate(Register, JobName).

%% Inspect specific statistic
job_stats(JobName) ->
    ecron:statistic(JobName).

%% Inspect all statistic
all_job_stats() ->
    ecron:statistic().

%% Predict latest N datetime.
predict_datetime_by_spec(Spec, N) ->
    ecron:parse_spec(Spec, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Manage Functions End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
