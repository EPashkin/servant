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
%% @doc Check directory to contains only archive with same name
%% ====================================================================
contains_only_archive(Dir) ->
    DirName = filename:basename(Dir),
    Files = filelib:wildcard("*", Dir),  
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
             meck:new(filelib, [unstick]),
             ok
     end,
     fun(_) -> 
             true = meck:validate(filelib),
             meck:unload(filelib)
     end,
     [
      fun(_) ->
              meck:expect(filelib, wildcard,  
                          fun ("*", _)-> ["test1.rar"] end),
              ?_assert(contains_only_archive("basedir/test1"))
      end,
      fun(_) ->
              meck:expect(filelib, wildcard,  
                          fun ("*", _)-> ["test.rar"] end),
              ?_assertNot(contains_only_archive("basedir/test1b"))
      end,
      fun(_) ->
              meck:expect(filelib, wildcard,  
                          fun ("*", _)-> ["dir", "test.rar"] end),
              ?_assertNot(contains_only_archive("basedir/test2"))
      end,
      fun(_) ->
              meck:expect(filelib, wildcard,  
                          fun ("*", _)-> ["dir", "test"] end),
              ?_assertNot(contains_only_archive("basedir/test3"))
      end,
      fun(_) ->
              meck:expect(filelib, wildcard,  
                          fun ("*", _)-> ["test.rar", "file"] end),
              ?_assertNot(contains_only_archive("basedir/test4"))
      end,
      fun(_) ->
              meck:expect(filelib, wildcard,  
                          fun ("*", _)-> [] end),
              ?_assertNot(contains_only_archive("basedir/test5"))
      end
     ]
    }.

%-endif.
