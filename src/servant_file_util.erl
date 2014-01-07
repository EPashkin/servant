%% @author Evgeny Pashkin
%% @doc Utils for working with files and directories


-module(servant_file_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         list_dir_subdirs/1,
         list_dir_all/1,
         is_file_archive/1
        ]).

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
%% @doc Return directory listing list, or [] on errors
%% ====================================================================
list_dir_all(Dir) ->
    case servant_file_proxy:list_dir_all(Dir) of
        {ok, Files} -> Files;
        _ -> []
    end.

%% ====================================================================
%% @doc Is filename is archive (by extensions)?
%% ====================================================================
is_file_archive(FileName) ->
    case filename:extension(FileName) of
        ".rar" -> true;
        ".zip" -> true;
        _ -> false
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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

list_dir_all_test_() ->
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
              ?_assertEqual({Expected, Files}, {list_dir_all(Dir), Files})
      end
      || {Expected, Files} <- 
             [
              {["dir", "file"], ["dir", "file"]},
              {[], {error, enoent}}, %dir not exists
              {[], []}
             ]]
    }.

is_file_archive_test_() ->
    [
     ?_assertNot(is_file_archive("test")),
     ?_assert(is_file_archive("test.rar")),
     ?_assert(is_file_archive("test.zip")),
     ?_assertNot(is_file_archive("test.erl"))
    ].

%-endif.
