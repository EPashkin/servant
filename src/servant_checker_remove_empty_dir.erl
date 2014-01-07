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
    servant_file_proxy:del_dir(Dir),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

is_empty_dir(Dir) ->
    case servant_file_proxy:list_dir_all(Dir) of
        {ok, []} -> true;
        _ -> false
    end.

%% ====================================================================
%% Tests
%% ====================================================================

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_empty_dir_test_() ->
    {
     foreach,
     fun() ->
             meck:new(servant_file_proxy),
             ok
     end,
     fun(_) ->
             true = meck:validate(servant_file_proxy),
             meck:unload(servant_file_proxy)
     end,
     [fun(_) -> 
              Dir = "basedir",
              DirRet = if
                           is_list(Files) -> {ok, Files};
                           true -> Files
                       end,
              meck:expect(servant_file_proxy, list_dir_all,
                          fun (Dir1) when Dir1 == Dir -> DirRet end),
              ?_assertEqual({Expected, Files}, {is_empty_dir(Dir), Files})
      end
      || {Expected, Files} <-
             [ 
              {false, ["test1.rar"]},
              {false, {error, enoent}}, %dir not exists
              {true, []}
             ]]
    }.

%-endif.
