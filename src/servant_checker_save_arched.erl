%% @author Evgeny Pashkin
%% @doc Checker task for move archives from subfolders

-module(servant_checker_save_arched).
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
    list_dir_subdirs(Dir).

check_subitem(SubDir) ->
    get_same_archive_in_directory(SubDir).

get_confirmations(SubDir, _CheckResult) ->
    DirName = filename:basename(SubDir),
    IOList = io_lib:format("Move from subfolder archive ~s", [DirName]),
    [#taskinfo{text=IOList, code={do_save_arched, SubDir}}].

do_subitem(_Dir, CheckResult) ->
    move_file_to_parent_directory(CheckResult).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% ====================================================================
%% @doc Return list of subdirectories
%% ====================================================================
list_dir_subdirs(Dir) ->
    {ok, Files} = servant_file_proxy:list_dir_all(Dir),
    Func = fun(SubDir) ->
                   FullDir = filename:join(Dir, SubDir),
                   case filelib:is_dir(FullDir) of
                       true -> {true, FullDir};
                       false -> false
                   end
           end,
    lists:filtermap(Func, Files).

%% ====================================================================
%% @doc Get archive with same name as directory, only if it contains only this archive
%% ====================================================================
get_same_archive_in_directory(Dir) ->
    DirName = filename:basename(Dir),
    {ok, Files} = servant_file_proxy:list_dir_all(Dir),
    Func = fun (_, false) -> false;
              (FileName, _Acc) ->
                   case is_file_archive(FileName) and
                            (filename:rootname(FileName) == DirName) of
                       false -> false;
                       true -> filename:join(Dir, FileName)
                   end
           end,
    case Files of
        [] -> false;
        _ ->lists:foldl(Func, true, Files)
    end.

move_file_to_parent_directory(File) ->
    NewFile = get_new_file_name(File),
    Res = servant_file_proxy:rename(File, NewFile),
    case Res of
        ok -> ok;
        {error, _Reason} -> ok
    end.

get_new_file_name(File) ->
    FileName = filename:basename(File),
    Dir = filename:dirname(File),
    ParentDir = filename:dirname(Dir),
    filename:join(ParentDir, FileName).

is_file_archive(FileName) ->
    case filename:extension(FileName) of
        ".rar" -> true;
        ".zip" -> true;
        _ -> false
    end.


%% ====================================================================
%% Tests
%% ====================================================================

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_file_archive_test_() ->
    [
     ?_assertNot(is_file_archive("test")),
     ?_assert(is_file_archive("test.rar")),
     ?_assert(is_file_archive("test.zip")),
     ?_assertNot(is_file_archive("test.erl"))
    ].

tf_getbasedir() -> "basedir".
tf_getsubdir() -> filename:join(tf_getbasedir(), "test1").
tf_getfiles() -> [
                  {filename:join(tf_getsubdir(), "test1.rar"), ["test1.rar"]},
                  {false, ["test.rar"]},
                  {false, ["dir", "test.rar"]},
                  {false, ["dir", "test"]},
                  {false, ["test.rar", "file"]},
                  {false, []}
                 ].

get_same_archive_in_directory_test_() ->
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
              meck:expect(servant_file_proxy, list_dir_all,
                          fun (Dir1) when Dir1 == Dir -> {ok, Files} end),
              ?_assertEqual({Expected, Files}, {get_same_archive_in_directory(Dir), Files})
      end
      || {Expected, Files} <- tf_getfiles()]
    }.

list_dir_subdirs_test_() ->
    {
     foreach,
     fun() ->
             meck:new(servant_file_proxy),
             meck:new(filelib, [unstick]),
             ok
     end,
     fun(_) ->
             true = meck:validate(filelib),
             meck:unload(filelib),
             true = meck:validate(servant_file_proxy),
             meck:unload(servant_file_proxy)
     end,
     [
      fun(_) ->
              meck:expect(servant_file_proxy, list_dir_all,
                          fun (_) -> {ok, ["file1", "file2", "dir1", "dir2"]} end),
              meck:expect(filelib, is_dir,
                          fun ("basedir/dir1") -> true;
                             ("basedir/dir2") -> true;
                             (_) -> false end),
              ?_assertEqual(["basedir/dir1", "basedir/dir2"], list_dir_subdirs("basedir"))
      end
     ]
    }.

get_new_file_name_test() ->
    ?assertEqual("basedir/dir1.rar", get_new_file_name("basedir/dir1/dir1.rar")).

get_confirmations_test_() ->
    [
     ?_assertMatch([#taskinfo{code={do_save_arched,"basedir/dir1"}}],
                   get_confirmations("basedir/dir1", not_used))
    ].

do_subitem_test_() ->
    {
     foreach,
     fun() ->
             meck:new(servant_file_proxy),
             
             meck:expect(servant_file_proxy, rename, 2, ok),
             ok
     end,
     fun(_) ->
             true = meck:validate(servant_file_proxy),
             meck:unload(servant_file_proxy)
     end,
     [
      fun(_) ->
              [
               ?_assertEqual(ok, do_subitem(not_used, "basedir/dir1/dir1.rar")),
               ?_assert(meck:called(servant_file_proxy, rename, ["basedir/dir1/dir1.rar", "basedir/dir1.rar"]))
              ]
      end
     ]
    }.

%-endif.
