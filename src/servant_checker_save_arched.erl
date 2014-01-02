%% @author Evgeny Pashkin
%% @doc Checker task for move archives from subfolders

-module(servant_checker_save_arched).
-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


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
%% @doc Check directory to contains only archive with same name
%% ====================================================================
contains_only_archive(Dir) ->
    DirName = filename:basename(Dir),
    {ok, Files} = servant_file_proxy:list_dir_all(Dir),
    Func = fun (_, false) -> false;
              (FileName, _Acc) ->
                   case is_file_archive(FileName) and
                            (filename:rootname(FileName) == DirName) of
                       false -> false;
                       true -> true
                   end
           end,
    case Files of
        [] -> false;
        _ ->lists:foldl(Func, true, Files)
    end.

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

contains_only_archive_test_() ->
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
     [
      fun(_) ->
              meck:expect(servant_file_proxy, list_dir_all,
                          fun (_) -> {ok, ["test1.rar"]} end),
              ?_assert(contains_only_archive("basedir/test1"))
      end,
      fun(_) ->
              meck:expect(servant_file_proxy, list_dir_all,
                          fun (_)-> {ok, ["test.rar"]} end),
              ?_assertNot(contains_only_archive("basedir/test1b"))
      end,
      fun(_) ->
              meck:expect(servant_file_proxy, list_dir_all,
                          fun (_)-> {ok, ["dir", "test.rar"]} end),
              ?_assertNot(contains_only_archive("basedir/test2"))
      end,
      fun(_) ->
              meck:expect(servant_file_proxy, list_dir_all,
                          fun (_)-> {ok, ["dir", "test"]} end),
              ?_assertNot(contains_only_archive("basedir/test3"))
      end,
      fun(_) ->
              meck:expect(servant_file_proxy, list_dir_all,
                          fun (_)-> {ok, ["test.rar", "file"]} end),
              ?_assertNot(contains_only_archive("basedir/test4"))
      end,
      fun(_) ->
              meck:expect(servant_file_proxy, list_dir_all,
                          fun (_)-> {ok, []} end),
              ?_assertNot(contains_only_archive("basedir/test5"))
      end
     ]
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

%-endif.
