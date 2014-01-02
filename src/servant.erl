%% @author Evgeny Pashkin
%% @doc Entry point for servant application


-module(servant).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         get_confirmations/0,
         process_confirmation/1,
         add_confirmation/2
        ]).

%% Return list of confirmations
get_confirmations() ->
    servant_tasklist:getForMenu().

%% Do confirmed work
process_confirmation(Code) ->
    servant_tasklist:doFromMenu(Code).

%% Add work to confirm
add_confirmation(Text,Code) ->
    servant_tasklist:addtask(Text,Code).
