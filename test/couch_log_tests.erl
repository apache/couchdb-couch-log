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

-module(couch_log_tests).

-include_lib("eunit/include/eunit.hrl").

callbacks_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_assertEqual({ok, couch_log_eunit}, couch_log:get_backend()),
            ?_assertEqual(ok, couch_log:set_level(info)),
            ?_assertEqual(ok, couch_log:debug("debug", [])),
            ?_assertEqual(ok, couch_log:info("info", [])),
            ?_assertEqual(ok, couch_log:notice("notice", [])),
            ?_assertEqual(ok, couch_log:warning("warning", [])),
            ?_assertEqual(ok, couch_log:error("error", [])),
            ?_assertEqual(ok, couch_log:critical("critical", [])),
            ?_assertEqual(ok, couch_log:alert("alert", [])),
            ?_assertEqual(ok, couch_log:emergency("emergency", [])),
            ?_assertEqual(stats_calls(), meck:history(couch_stats, self())),
            ?_assertEqual(log_calls(), meck:history(couch_log_eunit, self()))
        ]
    }.

setup() ->
    meck:new([couch_stats, couch_log_eunit], [non_strict]),
    meck:new(couch_log, [passthrough]),
    meck:expect(couch_stats, increment_counter, 1, ok),
    meck:expect(couch_log, get_backend, fun() -> application:get_env(couch_log, backend) end),
    setup_couch_log_eunit(),
    application:load(couch_log),
    application:set_env(couch_log, backend, couch_log_eunit).

cleanup(_) ->
    meck:unload([couch_stats, couch_log_eunit, couch_log]).

setup_couch_log_eunit() ->
    meck:expect(couch_log_eunit, set_level, 1, ok),
    Levels = [debug, info, notice, warning, error, critical, alert, emergency],
    lists:foreach(fun(Fun) ->
        meck:expect(couch_log_eunit, Fun, 2, ok)
    end, Levels).

stats_calls() ->
    Levels = [debug, info, notice, warning, error, critical, alert, emergency],
    lists:map(fun(Level) ->
        MFA = {couch_stats, increment_counter, [[couch_log, level, Level]]},
        {self(), MFA, ok}
    end, Levels).

log_calls() ->
    Levels = [debug, info, notice, warning, error, critical, alert, emergency],
    Calls = lists:map(fun(Level) ->
        MFA = {couch_log_eunit, Level, [atom_to_list(Level),[]]},
        {self(), MFA, ok}
    end, Levels),
    [{self(), {couch_log_eunit, set_level, [info]}, ok}|Calls].

