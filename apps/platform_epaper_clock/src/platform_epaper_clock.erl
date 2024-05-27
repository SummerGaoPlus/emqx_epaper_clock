%%%-------------------------------------------------------------------
%%% @author SummerGao
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 5月 2024 14:00
%%%-------------------------------------------------------------------
-module(platform_epaper_clock).
-author("SummerGao").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([send_command/1]).
-export([wake_up/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([handle_msg/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% 系统控制
%%%===================================================================
%% 握手指令
-define(HAND_SHAKE, "\xA5\x00\x09\x00\xCC\x33\xC3\x3C\xAC").
%% 模块返回当前波特率值，ASCII 码格式
-define(GET_BAUD_RATE, "\xA5\x00\x09\x02\xCC\x33\xC3\x3C\xAE").
%% 设置波特率
-define(SET_BAUD_RATE_9600, "\xA5\x00\x0D\x01\x00\x00\x25\x80\xCC\x33\xC3\x3C\x0C").
-define(SET_BAUD_RATE_19200, "\xA5\x00\x0D\x01\x00\x00\x4B\x00\xCC\x33\xC3\x3C\xE2").
-define(SET_BAUD_RATE_38400, "\xA5\x00\x0D\x01\x00\x00\x96\x00\xCC\x33\xC3\x3C\x3F").
-define(SET_BAUD_RATE_57600, "\xA5\x00\x0D\x01\x00\x00\xE1\x00\xCC\x33\xC3\x3C\x48").
-define(SET_BAUD_RATE_115200, "\xA5\x00\x0D\x01\x00\x01\xC2\x00\xCC\x33\xC3\x3C\x6A").

%% 返回当前使用的存储区 0: NandFlash 1: MicroSD
-define(GET_STORAGE_AREA, "\xA5\x00\x09\x06\xCC\x33\xC3\x3C\xAA").
%% 设置存储区(Micro-SD)
-define(SET_UP_STORAGE_AREA_MICRO_SD, "\xA5\x00\x0A\x07\x01\xCC\x33\xC3\x3C\xA9").
%% 设置存储区(NandFlash)
-define(SET_UP_STORAGE_AREA_MICRO_SD_NANDFLASH, "\xA5\x00\x0A\x07\x00\xCC\x33\xC3\x3C\xA8").
%% 系统睡眠 A5000908CC33C33CA4
-define(SYSTEM_SLEEP, "\xA5\x00\x09\x08\xCC\x33\xC3\x3C\xA4").
%% 立即刷新指令
-define(UPDATE, "\xA5\x00\x09\x0A\xCC\x33\xC3\x3C\xA6").
%% 返回当前的屏幕方向\x00: 正常，\x01: 90°旋转，0x02: 180°旋转，0x03: 270°旋转
-define(GET_SCREEN_DIRECTION, "\xA5\x00\x09\x0C\xCC\x33\xC3\x3C\xA0").
%% 设置屏幕旋转方向正常
-define(SET_SCREEN_DIRECTION_0, "\xA5\x00\x0A\x0D\x00\xCC\x33\xC3\x3C\xA2").
%% 设置屏幕旋转方向90°
-define(SET_SCREEN_DIRECTION_90, "\xA5\x00\x0A\x0D\x01\xCC\x33\xC3\x3C\xA3").
%% 设置屏幕旋转方向180°
-define(SET_SCREEN_DIRECTION_180, "\xA5\x00\x0A\x0D\x02\xCC\x33\xC3\x3C\xA4").
%% 设置屏幕旋转方向270°
-define(SET_SCREEN_DIRECTION_270, "\xA5\x00\x0A\x0D\x03\xCC\x33\xC3\x3C\xA5").
%% 载入字库
-define(LOAD_FONT, "\xA5\x00\x09\x0E\xCC\x33\xC3\x3C\xA2").
%% 载入图片
-define(LOAD_PICTURE, "\xA5\x00\x09\x0F\xCC\x33\xC3\x3C\xA3").

%%%===================================================================
%%% 显示参数配置
%%%===================================================================
%% 读取绘图颜色
-define(READ_DRAWING_COLOR, "\xA5\x00\x09\x11\xCC\x33\xC3\x3C\xBD").
%% 读取英文字号
-define(GET_ENGLISH_FONT_SIZE, "\xA5\x00\x0A\x1E\x01\xCC\x33\xC3\x3C\xB0").
%% 设置英文字号-32
-define(SET_ENGLISH_FONT_SIZE_32, "\xA5\x00\x0A\x1E\x01\xCC\x33\xC3\x3C\xB0").
-define(SET_ENGLISH_FONT_SIZE_48, "\xA5\x00\x0A\x1E\x02\xCC\x33\xC3\x3C\xB3").
-define(SET_ENGLISH_FONT_SIZE_64, "\xA5\x00\x0A\x1E\x03\xCC\x33\xC3\x3C\xB2 ").
%% 读取中文字号
-define(GET_CHINA_FONT_SIZE, "\xA5\x00\x09\x1D\xCC\x33\xC3\x3C\xB1").
%% 设置中文字号-32
-define(SET_CHINA_FONT_SIZE_32, "\xA5\x00\x0A\x1F\x01\xCC\x33\xC3\x3C\xB1").
-define(SET_CHINA_FONT_SIZE_48, "\xA5\x00\x0A\x1F\x02\xCC\x33\xC3\x3C\xB2").
-define(SET_CHINA_FONT_SIZE_64, "\xA5\x00\x0A\x1F\x03\xCC\x33\xC3\x3C\xB3").

%%%===================================================================
%%% 基本绘图
%%%===================================================================
%% 清屏指令
-define(CLEAR_SCREEN, "\xA5\x00\x09\x2E\xCC\x33\xC3\x3C\x82").

%% 帧头
-define(FH, "A5").
%% 结尾
-define(END, "00").
%% 帧尾
-define(FT, "CC33C33C").
%% XY坐标
-define(XY, "00000000").

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

send_command(Command) ->
    gen_server:call(?MODULE, Command).

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
    init(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% gen_server:call(platform_epaper_clock,{display_char,{100,100},<<"SummerGao hello 你好 哈哈哈"/utf8>>}).
%% gen_server:call(platform_epaper_clock,{display_char,{100,100},"SummerGao hello 你好 哈哈哈"}).
%% gen_server:call(platform_epaper_clock,{display_image,{100,180},"PIC2.BMP"}).
%% gen_server:call(platform_epaper_clock,load_picture).
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
handle_call(start, _From, State) ->
    init(),
    {reply, ok, State};
handle_call(update, _From, State) ->
    update(),
    {reply, ok, State};
handle_call(system_sleep, _From, State) ->
    system_sleep(),
    {reply, ok, State};
handle_call(set_screen_direction180, _From, State) ->
    set_screen_direction180(),
    {reply, ok, State};
handle_call(set_up_storage_area_micro_sd_nandflash, _From, State) ->
    set_up_storage_area_micro_sd_nandflash(),
    {reply, ok, State};
handle_call(set_english_font_size_32, _From, State) ->
    set_english_font_size_32(),
    {reply, ok, State};
handle_call(set_english_font_size_48, _From, State) ->
    set_english_font_size_48(),
    {reply, ok, State};
handle_call(set_english_font_size_64, _From, State) ->
    set_english_font_size_64(),
    {reply, ok, State};
handle_call(set_china_font_size_32, _From, State) ->
    set_china_font_size_32(),
    {reply, ok, State};
handle_call(set_china_font_size_48, _From, State) ->
    set_china_font_size_48(),
    {reply, ok, State};
handle_call(set_china_font_size_64, _From, State) ->
    set_china_font_size_64(),
    {reply, ok, State};
handle_call(set_up_storage_area_micro_sd, _From, State) ->
    set_up_storage_area_micro_sd(),
    {reply, ok, State};
handle_call(load_picture, _From, State) ->
    load_picture(),
    {reply, ok, State};
handle_call(load_font, _From, State) ->
    load_font(),
    {reply, ok, State};
handle_call(clear_screen, _From, State) ->
    clear_screen(),
    {reply, ok, State};
handle_call({display_char, Msg}, _From, State) ->
    display(displayTextOrImage("30", {0, 0}, Msg)),
    {reply, ok, State};
handle_call({display_char, {X, Y}, Msg}, _From, State) ->
    display(displayTextOrImage("30", {X, Y}, Msg)),
    {reply, ok, State};
handle_call({clear_display_char, {X, Y}, Msg}, _From, State) ->
    clear_screen(),
    display(displayTextOrImage("30", {X, Y}, Msg)),
    {reply, ok, State};
handle_call({display_image, Msg}, _From, State) ->
    display(displayTextOrImage("70", {0, 0}, Msg)),
    {reply, ok, State};
handle_call({display_image, {X, Y}, Msg}, _From, State) ->
    display(displayTextOrImage("70", {X, Y}, Msg)),
    {reply, ok, State};
%% 绘制图形
handle_call({draw_base_straight_line, {X0, Y0, X1, Y1}}, _From, State) ->
    display(draw_base("22", {X0, Y0, X1, Y1})),
    {reply, ok, State};
handle_call({draw_base_rectangle_filled, {X0, Y0, X1, Y1}}, _From, State) ->
    display(draw_base("24", {X0, Y0, X1, Y1})),
    {reply, ok, State};
handle_call({draw_base_rectangle, {X0, Y0, X1, Y1}}, _From, State) ->
    display(draw_base("25", {X0, Y0, X1, Y1})),
    {reply, ok, State};
handle_call({draw_circle, {X0, Y0, R}}, _From, State) ->
    display(draw_circle("26", {X0, Y0, R})),
    {reply, ok, State};
handle_call({draw_circle_filled, {X0, Y0, R}}, _From, State) ->
    display(draw_circle("27", {X0, Y0, R})),
    {reply, ok, State};
handle_call({draw_triangle, {X0, Y0, X1, Y1, X2, Y2}}, _From, State) ->
    display(draw_triangle("28", {X0, Y0, X1, Y1, X2, Y2})),
    {reply, ok, State};
handle_call({draw_triangle_filled, {X0, Y0, X1, Y1, X2, Y2}}, _From, State) ->
    display(draw_triangle("29", {X0, Y0, X1, Y1, X2, Y2})),
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
    io:format("_Info : ~p~n", [_Info]),
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

init() ->
    %% 唤醒
    wake_up(1),

    timer:sleep(10),
    handshake(),
    timer:sleep(10),
    clear_screen(),
    timer:sleep(10),
    update(),

%%  set_up_storage_area_micro_sd(),
%%  load_picture(),
%%  load_font(),
    set_english_font_size_48(),
    set_china_font_size_48(),
%%  update(),
%%  timer:sleep(3000),
%%  set_up_storage_area_micro_sd_nandflash(),
    platform_gateway:send_msg("\xA5\x00\x2F\x30\x00\x00\x00\x00\xC4\xFA\xBA\xC3\xA3\xA1\xBB\xB6\xD3\xAD\xCA\xB9\xD3\xC3\xA3\xAC\x53\x75\x6D\x6D\x65\x72\x47\x61\x6F\xD6\xC7\xBB\xDB\xC6\xC1\xA3\xA1\x00\xCC\x33\xC3\x3C\xEC"),
    display(displayTextOrImage("30", {320, 120}, "香橙派AIPro")),
    display(displayTextOrImage("70", {200, 200}, "PIC3.BMP")),
    update(),
    timer:sleep(10),
    %% 休眠
    system_sleep(),
    wake_up(0).

%% 握手
handshake() ->
    ok = platform_gateway:send_msg(?HAND_SHAKE).

%% 清屏
clear_screen() ->
    ok = platform_gateway:send_msg(?CLEAR_SCREEN).

%% 刷新
update() ->
    ok = platform_gateway:send_msg(?UPDATE).

%% 休眠
system_sleep() ->
    ok = platform_gateway:send_msg(?SYSTEM_SLEEP).

%% 设置屏幕旋转方向180°
set_screen_direction180() ->
    ok = platform_gateway:send_msg(?SET_SCREEN_DIRECTION_180).

%% 设置英文字号-32
set_english_font_size_32() ->
    ok = platform_gateway:send_msg(?SET_ENGLISH_FONT_SIZE_32).

%% 设置中文字号-32
set_china_font_size_32() ->
    ok = platform_gateway:send_msg(?SET_CHINA_FONT_SIZE_32).

%% 设置英文字号-48
set_english_font_size_48() ->
    ok = platform_gateway:send_msg(?SET_ENGLISH_FONT_SIZE_48).

%% 设置中文字号-48
set_china_font_size_48() ->
    ok = platform_gateway:send_msg(?SET_CHINA_FONT_SIZE_48).

%% 设置英文字号-64
set_english_font_size_64() ->
    ok = platform_gateway:send_msg(?SET_ENGLISH_FONT_SIZE_64).

%% 设置中文字号-64
set_china_font_size_64() ->
    ok = platform_gateway:send_msg(?SET_CHINA_FONT_SIZE_64).

%% 设置存储区域为内存卡
set_up_storage_area_micro_sd() ->
    ok = platform_gateway:send_msg(?SET_UP_STORAGE_AREA_MICRO_SD).
%% 设置存储区域为FLASH
set_up_storage_area_micro_sd_nandflash() ->
    ok = platform_gateway:send_msg(?SET_UP_STORAGE_AREA_MICRO_SD_NANDFLASH).

%% 载入字库
load_font() ->
    ok = platform_gateway:send_msg(?LOAD_FONT).

%% 载入图片
load_picture() ->
    ok = platform_gateway:send_msg(?LOAD_PICTURE).

%% 显示
display(Msg) ->
    platform_gateway:send_msg(Msg).

%% 处理数据
%% 消息格式： {"command":"display_char","position":[10,20],"msg":"我是高正"}
handle_msg(Msg) ->
    io:format("Msg: ~ts~n", [Msg]),
    io:format("Msg: ~p~n", [jsx:decode(Msg, [return_maps, {labels, atom}])]),
    case catch jsx:decode(Msg, [return_maps, {labels, atom}]) of
        Map when is_map(Map) ->
            case Map of
                #{'command' := Command} ->
                    case Command of
                        <<"update">> ->
                            gen_server:call(?MODULE, update);
                        <<"set_up_storage_area_micro_sd">> ->
                            gen_server:call(?MODULE, set_up_storage_area_micro_sd);
                        <<"load_picture">> ->
                            gen_server:call(?MODULE, load_picture);
                        <<"load_font">> ->
                            gen_server:call(?MODULE, load_font);
                        <<"clear_screen">> ->
                            gen_server:call(?MODULE, clear_screen);
                        <<"set_english_font_size_32">> ->
                            gen_server:call(?MODULE, set_english_font_size_32);
                        <<"set_english_font_size_48">> ->
                            gen_server:call(?MODULE, set_english_font_size_48);
                        <<"set_english_font_size_64">> ->
                            gen_server:call(?MODULE, set_english_font_size_64);
                        <<"set_china_font_size_32">> ->
                            gen_server:call(?MODULE, set_china_font_size_32);
                        <<"set_china_font_size_48">> ->
                            gen_server:call(?MODULE, set_china_font_size_48);
                        <<"set_china_font_size_64">> ->
                            gen_server:call(?MODULE, set_china_font_size_64);
                        <<"display_char">> ->
                            Position = maps:get(position, Map, [0, 0]),
                            [X, Y] = Position,
                            #{'msg' := M} = Map,
                            gen_server:call(?MODULE, {display_char, {X, Y}, M});
                        <<"clear_display_char">> ->
                            Position = maps:get(position, Map, [0, 0]),
                            [X, Y] = Position,
                            #{'msg' := M} = Map,
                            gen_server:call(?MODULE, {clear_display_char, {X, Y}, M});
                        <<"display_image">> ->
                            Position = maps:get(position, Map, [0, 0]),
                            [X, Y] = Position,
                            #{'msg' := M} = Map,
                            gen_server:call(?MODULE, {display_image, {X, Y}, M});
                        <<"draw_base_straight_line">> ->
                            Position = maps:get(position, Map, [0, 0, 0, 0]),
                            [X0, Y0, X1, Y1] = Position,
                            gen_server:call(?MODULE, {draw_base_straight_line, {X0, Y0, X1, Y1}});
                        <<"draw_base_rectangle">> ->
                            Position = maps:get(position, Map, [0, 0, 0, 0]),
                            [X0, Y0, X1, Y1] = Position,
                            gen_server:call(?MODULE, {draw_base_rectangle, {X0, Y0, X1, Y1}});
                        <<"draw_base_rectangle_filled">> ->
                            Position = maps:get(position, Map, [0, 0, 0, 0]),
                            [X0, Y0, X1, Y1] = Position,
                            gen_server:call(?MODULE, {draw_base_rectangle_filled, {X0, Y0, X1, Y1}});
                        <<"draw_circle">> ->
                            Position = maps:get(position, Map, [0, 0, 0]),
                            [X0, Y0, R] = Position,
                            gen_server:call(?MODULE, {draw_circle, {X0, Y0, R}});
                        <<"draw_circle_filled">> ->
                            Position = maps:get(position, Map, [0, 0, 0]),
                            [X0, Y0, R] = Position,
                            gen_server:call(?MODULE, {draw_circle_filled, {X0, Y0, R}});
                        <<"draw_triangle">> ->
                            Position = maps:get(position, Map, [0, 0, 0, 0, 0, 0]),
                            [X0, Y0, X1, Y1, X2, Y2] = Position,
                            gen_server:call(?MODULE, {draw_triangle, {X0, Y0, X1, Y1, X2, Y2}});
                        <<"draw_triangle_filled">> ->
                            Position = maps:get(position, Map, [0, 0, 0, 0, 0, 0]),
                            [X0, Y0, X1, Y1, X2, Y2] = Position,
                            gen_server:call(?MODULE, {draw_triangle_filled, {X0, Y0, X1, Y1, X2, Y2}});
                        _Else ->
                            io:format("Error: ~p~n", ["command is undefined"])
                    end;
                _Else ->
                    io:format("Error: ~p~n", ["command is undefined"])
            end;
        List when is_list(List) ->
            io:format("List : ~p~n", [List]);
        _Else ->
            io:format("Error : ~p~n", ["Bad JSON Msg"])
    end.

%% 文字显示/图片显示
-spec(displayTextOrImage(Cmd :: string(), {integer(), integer()}, Msg :: string()) ->
    string()).
displayTextOrImage(Cmd, {X0, Y0}, Msg) ->
    % 二进制消息转16进制
    HexMsg = platform_utils:binary_to_hex_str(
        if
            is_binary(Msg) ->
                iconv:convert(<<"utf-8">>, <<"gbk">>, Msg);
            is_list(Msg) ->
                B = unicode:characters_to_binary(Msg, utf8),
                iconv:convert(<<"utf-8">>, <<"gbk">>, B);
            true ->
                iolist_to_binary(Msg)
        end
    ),
    % 消息长度
    Len = string:len(HexMsg),
    % 转字节长度
    Byte = Len div 2,
    % 计算总长度
    TotalLen = 1 + 2 + 1 + 4 + Byte + 1 + 4 + 1,
    % 帧长
    FL = integer_to_list(TotalLen, 16),
    %% 帧长不够4位前面自动补0
    FL2 = string:right(FL, 4, $0),
    Command = ?FH ++ FL2 ++ Cmd ++ format(X0) ++ format(Y0) ++ HexMsg ++ ?END ++ ?FT,
    build_command(Command).


%% 基本绘图
-spec(draw_base(Cmd :: string(), {integer(), integer(), integer(), integer()}) ->
    string()).
draw_base(Cmd, {X0, Y0, X1, Y1}) ->
    Command = ?FH ++ "0011" ++ Cmd ++ format(X0) ++ format(Y0) ++ format(X1) ++ format(Y1) ++ ?FT,
    build_command(Command).

%% 画圆/填充圆
draw_circle(Cmd, {X0, Y0, R}) ->
    %% 指令
    Command = ?FH ++ "000F" ++ Cmd ++ format(X0) ++ format(Y0) ++ format(R) ++ ?FT,
    build_command(Command).

%% 画三角形/填充三角形
draw_triangle(Cmd, {X0, Y0, X1, Y1, X2, Y2}) ->
    Command = ?FH ++ "0015" ++ Cmd ++ format(X0) ++ format(Y0) ++ format(X1) ++ format(Y1) ++ format(X2) ++ format(Y2) ++ ?FT,
    build_command(Command).


%% 格式化4位2字节16进制字符串，不够4位在前面补0
format(X) ->
    string:right(integer_to_list(X, 16), 4, $0).

%% 构建输出指令
build_command(Command) ->
    %% 异或校验
    Check = lists:foldl(fun(X, R) ->
        R bxor list_to_integer(X, 16) end, 0, string_slice_to_list(Command, [])),
    Check2 = erlang:integer_to_list(Check, 16),
    Command2 = Command ++ Check2,
    [list_to_integer(X, 16) || X <- string_slice_to_list(Command2, [])].


%% 字符串以两个字符为单位从左到右抽取成字符串列表
-spec(string_slice_to_list(S :: string(), R :: list()) ->
    list()).
string_slice_to_list([], R) ->
    R;
string_slice_to_list(S, R) ->
    H = string:slice(S, 0, 2),
    L = string:slice(S, 2),
    T = R ++ [H],
    string_slice_to_list(L, T).

wake_up(0) ->
    platform_client_mqtt:publish_msg(<<"home/devices/wake_up/">>, "WAKE_UP_0"),
    %% GPIO 组号为 2， GPIO 管脚号为 20， 设置其方向为输出
    os:cmd("gpio_operate set_direction 2 20 1"),
    %% GPIO 组号为 2， GPIO 管脚号为 20， 设置其输出为低电平
    os:cmd("gpio_operate set_value 2 20 0");
wake_up(1) ->
    platform_client_mqtt:publish_msg(<<"home/devices/wake_up/">>, "WAKE_UP_1"),
    %% GPIO 组号为 2， GPIO 管脚号为 20， 设置其方向为输出
    os:cmd("gpio_operate set_direction 2 20 1"),
    %% GPIO 组号为 2， GPIO 管脚号为 20， 设置其输出为高电平
    os:cmd("gpio_operate set_value 2 20 1").
