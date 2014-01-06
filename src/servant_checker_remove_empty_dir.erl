%% @author Evgeny Pashkin
%% @doc Checker task for removing empty subdirectories

-module(servant_checker_remove_empty_dir).
-behaviour(servant_checker).
-export([get_subitems/1, check_subitem/1, get_confirmations/2, do_subitem/2]).

-include("internal.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

get_subitems(Dir) ->
    servant_file_util:list_dir_subdirs(Dir).

check_subitem(SubDir) ->
    is_empty_dir(SubDir).

get_confirmations(SubDir, _CheckResult) ->
    DirName = filename:basename(SubDir),
    IOList = io_lib:format("Remove empty subfolder ~s", [DirName]),
    [#taskinfo{text=IOList, code={do_remove_empty_dir, SubDir}}].

do_subitem(Dir, _CheckResult) ->
    file:del_dir(Dir).

%% ====================================================================
%% Internal functions
%% ====================================================================

is_empty_dir(Dir) ->
    Files = case servant_file_proxy:list_dir_all(Dir) of
                {ok, Files2} -> Files2;
                _ -> []
            end,
    case Files of
        [] -> true;
        _ -> false
    end.
