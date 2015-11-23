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

-module(couch_log_devnull).

%% Useful for suppressing all logging output while running tests,
%% irrespective of backend

-behaviour(couch_log).

-export([
    debug/2,
    info/2,
    notice/2,
    warning/2,
    error/2,
    critical/2,
    alert/2,
    emergency/2,
    set_level/1
]).

debug(_Fmt, _Args) ->
    ok.

info(_Fmt, _Args) ->
    ok.

notice(_Fmt, _Args) ->
    ok.

warning(_Fmt, _Args) ->
    ok.

error(_Fmt, _Args) ->
    ok.

critical(_Fmt, _Args) ->
    ok.

alert(_Fmt, _Args) ->
    ok.

emergency(_Fmt, _Args) ->
    ok.

set_level(_) ->
    ok.
