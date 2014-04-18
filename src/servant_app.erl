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
    security_allow(),
    ?SUP:start_link().

stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

security_allow() ->
    CfgAllowed = application:get_env(?APP, 'allowed', []),
    io:format("~p~n",[CfgAllowed]),
    AllowedNodes = translate_config_allowed(CfgAllowed),
    net_kernel:allow(AllowedNodes).

translate_config_allowed([]) ->
    [];
translate_config_allowed([Name | Tail]) when is_list(Name) ->
    NodeNameString = 
        case lists:member($@, Name) of
            true-> Name;
            false -> CompName = net_adm:localhost(),
                     Name ++ "@" ++ CompName
        end,
    NodeName = list_to_atom(NodeNameString),
    [NodeName | translate_config_allowed(Tail)].

%% ===================================================================
%% Tests
%% ===================================================================

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    ?assert(lists:member($@, "test@host")),
    ok = application:start(?APP),
    ?assertNot(undefined == whereis(?SUP)),
    ok = application:stop(?APP),
    ?assert(undefined == whereis(?SUP)).

translate_config_allowed_test_() ->
    [?_assertEqual({Expected, Value}, {translate_config_allowed(Value), Value})
     || {Expected, Value} <- 
            [
             {[], []},
             {[test@host], ["test@host"]},
             {[list_to_atom("test@"++net_adm:localhost())], ["test"]},
             {[test@host, test1@host], ["test@host", "test1@host"]}
            ]].

%-endif.
