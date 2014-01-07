%% @author Evgeny Pashkin
%% @doc Checker for recheck all confirmation

-module(servant_checker_recheck_confirmations).
-behaviour(servant_checker).
-export([get_subitems/1, check_subitem/1, get_confirmations/2, do_subitem/2]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

get_subitems(_Dir) ->
    servant_confirmation_list:recheck_confirmations(),
    [].

check_subitem(_SubDir) ->
    false.

get_confirmations(_SubDir, _CheckResult) ->
    [].

do_subitem(_Dir, _CheckResult) ->
    ok.
