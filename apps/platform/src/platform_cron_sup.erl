%%--------------------------------------------------------------------
% Copyright (c) 2024-2025 SummerGao <summergao@vip.qq.com>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%%--------------------------------------------------------------------
-module(platform_cron_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => 100,
    period => 10
  },
  CrontabName = platform_cron,
  Jobs = application:get_env(platform, crontab_jobs, []),
  Crontab =
    #{
      id => CrontabName,
      start => {ecron, start_link, [{local, CrontabName}, Jobs]},
      restart => permanent,
      shutdown => 1000,
      type => worker,
      modules => [CrontabName]
    },
  {ok, {SupFlags, [Crontab]}}.
