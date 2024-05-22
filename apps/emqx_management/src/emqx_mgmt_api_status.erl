%%--------------------------------------------------------------------
%% Copyright (c) 2020-2024 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-module(emqx_mgmt_api_status).

-behaviour(minirest_api).

-include_lib("hocon/include/hoconsc.hrl").

%% minirest API
-export([api_spec/0, paths/0, schema/1]).

-export([get_status/2]).

-export([
    init/2,
    path/0
]).

-define(TAGS, [<<"Status">>]).

%%--------------------------------------------------------------------
%% minirest API and schema
%%--------------------------------------------------------------------

api_spec() ->
    emqx_dashboard_swagger:spec(?MODULE, #{check_schema => true}).

paths() ->
    ["/status"].

schema("/status") ->
    #{
        'operationId' => get_status,
        get => #{
            parameters => [
                {format,
                    hoconsc:mk(
                        string(),
                        #{
                            in => query,
                            default => <<"text">>,
                            desc => ?DESC(get_status_api_format)
                        }
                    )}
            ],
            description => ?DESC(get_status_api),
            tags => ?TAGS,
            security => [],
            responses => #{
                200 => ?DESC(get_status_response200),
                503 => ?DESC(get_status_response503)
            }
        }
    }.

%%--------------------------------------------------------------------
%% non-minirest (cowboy) API
%%--------------------------------------------------------------------

%% Note: Because swagger now requires an HTTP prefix (e.g. /api/v5),
%% but the `/status` does not require this fixed prefix.
%%
%% Changing the swagger framework was too big, so we implemented the `/status`
%% in a simple way first
%%
%% XXX: So the HTTP API docs generated by swagger can not find this API now
path() ->
    "/status".

init(Req0, State) ->
    Format =
        try
            QS = cowboy_req:parse_qs(Req0),
            {_, F} = lists:keyfind(<<"format">>, 1, QS),
            F
        catch
            _:_ ->
                <<"text">>
        end,
    {Code, Headers, Body} = running_status(Format),
    Req = cowboy_req:reply(Code, Headers, Body, Req0),
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% API Handler funcs
%%--------------------------------------------------------------------

get_status(get, Params) ->
    Format = maps:get(<<"format">>, maps:get(query_string, Params, #{}), <<"text">>),
    running_status(iolist_to_binary(Format)).

running_status(Format) ->
    case emqx_dashboard_listener:is_ready(timer:seconds(20)) of
        true ->
            AppStatus = application_status(),
            Body = do_get_status(AppStatus, Format),
            StatusCode =
                case AppStatus of
                    running -> 200;
                    not_running -> 503
                end,
            ContentType =
                case Format of
                    <<"json">> -> <<"applicatin/json">>;
                    _ -> <<"text/plain">>
                end,
            Headers = #{
                <<"content-type">> => ContentType,
                <<"retry-after">> => <<"15">>
            },
            {StatusCode, Headers, iolist_to_binary(Body)};
        false ->
            {503, #{<<"retry-after">> => <<"15">>}, <<>>}
    end.

do_get_status(AppStatus, <<"json">>) ->
    BrokerStatus = broker_status(),
    emqx_utils_json:encode(#{
        node_name => atom_to_binary(node(), utf8),
        rel_vsn => vsn(),
        broker_status => atom_to_binary(BrokerStatus),
        app_status => atom_to_binary(AppStatus)
    });
do_get_status(AppStatus, _) ->
    BrokerStatus = broker_status(),
    io_lib:format("Node ~ts is ~ts~nemqx is ~ts", [node(), BrokerStatus, AppStatus]).

vsn() ->
    iolist_to_binary([
        emqx_release:edition_vsn_prefix(),
        emqx_release:version()
    ]).

broker_status() ->
    case emqx:is_running() of
        true ->
            started;
        false ->
            stopped
    end.

application_status() ->
    case lists:keysearch(emqx, 1, application:which_applications()) of
        false -> not_running;
        {value, _Val} -> running
    end.
