% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_log).

-include_lib("eunit/include/eunit.hrl").

-export([debug/2, info/2, notice/2, warning/2, error/2, critical/2, alert/2, emergency/2]).
-export([set_level/1, get_backend/0]).

-callback debug(Fmt::string(), Args::list()) -> ok.
-callback info(Fmt::string(), Args::list()) -> ok.
-callback notice(Fmt::string(), Args::list()) -> ok.
-callback warning(Fmt::string(), Args::list()) -> ok.
-callback error(Fmt::string(), Args::list()) -> ok.
-callback critical(Fmt::string(), Args::list()) -> ok.
-callback alert(Fmt::string(), Args::list()) -> ok.
-callback set_level(Level::atom()) -> ok.


-spec debug(string(), list()) -> ok.
debug(Fmt, Args) ->
    {ok, Backend} = ?MODULE:get_backend(),
    catch couch_stats:increment_counter([couch_log, level, debug]),
    Backend:debug(Fmt, Args).

-spec info(string(), list()) -> ok.
info(Fmt, Args) ->
    {ok, Backend} = ?MODULE:get_backend(),
    catch couch_stats:increment_counter([couch_log, level, info]),
    Backend:info(Fmt, Args).

-spec notice(string(), list()) -> ok.
notice(Fmt, Args) ->
    {ok, Backend} = ?MODULE:get_backend(),
    catch couch_stats:increment_counter([couch_log, level, notice]),
    Backend:notice(Fmt, Args).

-spec warning(string(), list()) -> ok.
warning(Fmt, Args) ->
    {ok, Backend} = ?MODULE:get_backend(),
    catch couch_stats:increment_counter([couch_log, level, warning]),
    Backend:warning(Fmt, Args).

-spec error(string(), list()) -> ok.
error(Fmt, Args) ->
    {ok, Backend} = ?MODULE:get_backend(),
    catch couch_stats:increment_counter([couch_log, level, 'error']),
    Backend:error(Fmt, Args).

-spec critical(string(), list()) -> ok.
critical(Fmt, Args) ->
    {ok, Backend} = ?MODULE:get_backend(),
    catch couch_stats:increment_counter([couch_log, level, critical]),
    Backend:critical(Fmt, Args).

-spec alert(string(), list()) -> ok.
alert(Fmt, Args) ->
    {ok, Backend} = ?MODULE:get_backend(),
    catch couch_stats:increment_counter([couch_log, level, alert]),
    Backend:alert(Fmt, Args).

-spec emergency(string(), list()) -> ok.
emergency(Fmt, Args) ->
    {ok, Backend} = ?MODULE:get_backend(),
    catch couch_stats:increment_counter([couch_log, level, emergency]),
    Backend:emergency(Fmt, Args).

-spec set_level(atom()) -> ok.
set_level(Level) ->
    {ok, Backend} = ?MODULE:get_backend(),
    Backend:set_level(Level).

-spec get_backend() -> {ok, atom()}.
-ifdef(TEST).
get_backend() ->
    {ok, couch_log_testbackend}.
-else.
get_backend() ->
    application:get_env(?MODULE, backend).
-endif.


