%%--------------------------------------------------------------------
% Copyright (c) 2024-2025 SummerGao <summergao@vip.qq.com>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%%--------------------------------------------------------------------
-module(platform_serials_sup).
-author("SummerGao").

-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).
-export([stop/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() -> exit(whereis(?MODULE), shutdown).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

%% 1、重启元组中的最后两个元素分别是 MaxRestart 和 MaxTime。 MaxRestart 指定了在
%% MaxTime 秒中所有子进程的最大重启次数。如果在这一秒数范围内达到最大重启次数，
%% 监督者本身就会被终止（原因标记为 shutdown ），而该终止行为又会传递给上级的监督
%% 者 。
%% 2、重启策略:
%% 1) one_for_one: 如果一个子进程停止，则只重启该进程;
%% 2) one_for_all: 如果一个子进程停止，所有其他子进程也停止，然后所有进程重启;
%% 3) rest_for_one: 如果一个子进程停止，则启动顺序中在它之后的所有其他子进程也停止，
%% 然后停止的这些进程重启（跟楼上那位不一样）;
%% 4) simple_one_for_one: 一个简化的one_for_one supervisor，所有的子进程都是同样进程
%% 类型并且是动态添加的实例 .
init(_) ->
  ChildSpecList = [child(platform_serials)],
  SupFlags = #{strategy => rest_for_one, intensity => 10, period => 3600},
  {ok, {SupFlags, ChildSpecList}}.

%% 1、restart定义了子进程什么时候重启:
%% 1) transient: 只有子进程是非正常终止时才重新启动。
%% 2) permanent: 表示子进程始终重启
%% 3) temporary: 表示子进程决不重启
%% 2、ShutdownTime 是一个正整数，表示以毫秒为单位的时间，或原子 infinity。
%% 它表示了从监督者发出 EXIT 信号到 terminate 回调函数返回之间允许的最长时间。
child(Module) ->
  #{id => Module, start => {Module, start_link, []},
    restart => transient, shutdown => 2000, type => worker, modules => [Module]}.
