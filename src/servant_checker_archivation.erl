%% @author Evgeny Pashkin
%% @doc Checker task for archivation data in subfolders

-module(servant_checker_archivation).
-behaviour(servant_checker).
-export([get_subitems/1, check_subitem/1, get_confirmations/2, do_subitem/2]).

-include("internal.hrl").
-compile([export_all]).

%% ====================================================================
%% API functions
%% ====================================================================

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

get_subitems(Dir) ->
    servant_file_util:list_dir_subdirs(Dir).

check_subitem(SubDir) ->
    can_be_archived(SubDir).

get_confirmations(SubDir, _CheckResult) ->
    DirName = filename:basename(SubDir),
    IOList = io_lib:format("Archive subfolder ~s", [DirName]),
    [#taskinfo{text=IOList, code={do_archivation, SubDir}}].

do_subitem(Dir, _CheckResult) ->
    archive_directory(Dir).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% ====================================================================
%% @doc Is directory contains any archive?
%% ====================================================================
can_be_archived(Dir) ->
    Files = case servant_file_proxy:list_dir_all(Dir) of
                {ok, Files2} -> Files2;
                _ -> []
            end,
    Func = fun (_, false) -> false;
              (FileName, _Acc) -> not servant_file_util:is_file_archive(FileName)
           end,
    case Files of
        [] -> false;
        _ ->    lists:foldl(Func, true, Files)
    end.

%% ====================================================================
%% @doc Archive all directory files
%% ====================================================================
archive_directory(Dir) ->
    FileName = filename:basename(Dir) ++ ".rar",
    {ok, OldDir} = file:get_cwd(),
    file:set_cwd(Dir),
    Command = ["start winrar", " m", " -ibck -r -s- ", FileName],
    os:cmd(Command),
    file:set_cwd(OldDir),
    ok.

%% ====================================================================
%% Tests
%% ====================================================================

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

tf_getbasedir() -> "basedir".
tf_getsubdir() -> filename:join(tf_getbasedir(), "test1").
tf_getfiles() -> [
                  {false, ["test.rar"]},
                  {false, ["dir", "test.rar"]},
                  {true, ["dir", "file"]},
                  {false, ["test.rar", "file"]},
                  {false, {error, enoent}}, %dir not exists
                  {false, []}
                 ].

contains_archive_test_() ->
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
              Dir = tf_getsubdir(),
              DirRet = if
                           is_list(Files) -> {ok, Files};
                           true -> Files
                       end,
              meck:expect(servant_file_proxy, list_dir_all,
                          fun (Dir1) when Dir1 == Dir -> DirRet end),
              ?_assertEqual({Expected, Files}, {can_be_archived(Dir), Files})
      end
      || {Expected, Files} <- tf_getfiles()]
    }.

%-endif.
