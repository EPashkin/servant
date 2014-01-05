-module(servant_app).

-behaviour(application).

-include("internal.hrl").
-define(SUP, servant_sup).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?SUP:start_link().

stop(_State) ->
    ok.


%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    ok = application:start(?APP),
    ?assertNot(undefined == whereis(?SUP)),
    ok = application:stop(?APP),
    ?assert(undefined == whereis(?SUP)).

-endif.
