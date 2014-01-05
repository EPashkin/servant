%% @author Evgeny Pashkin
%% @doc Entry point for servant application


-module(servant).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         get_confirmations/0,
         process_confirmation/1,
         add_confirmation/3
        ]).

%% Return list of confirmations
get_confirmations() ->
    servant_confirmation_list:get_confirmations().

%% Do confirmed work
process_confirmation(Code) ->
    servant_confirmation_list:process_confirmation(Code).

%% Add work to confirm
add_confirmation(Text, Code, Module) ->
    servant_confirmation_list:add_confirmation(Text, Code, Module).
