%% @author Evgeny Pashkin
%% @doc Utils (primary for testing)

-module(servant_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         get_dir_files/1
        ]).

get_dir_files(Dir) ->
    {ok, Files} = file:list_dir_all(Dir),
    Files.

%% ====================================================================
%% Internal functions
%% ====================================================================


