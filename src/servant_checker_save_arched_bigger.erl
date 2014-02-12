%% @author Evgeny Pashkin
%% @doc Checker task for automatically move bigger new archives from subfolders

-module(servant_checker_save_arched_bigger).
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

%add task, if condition is ok
check_subitem(SubDir) ->
    case servant_checker_save_arched:get_same_archive_in_directory(SubDir) of
        false -> false;
        FileName ->
            case is_new_archive_bigger_then_old(FileName) of
                false -> false;
                true -> 
                    servant_task_queue_manager:in(#task{code={do_save_arched_bigger, SubDir}}),
                    FileName
            end
    end.

get_confirmations(_SubDir, _CheckResult) ->
    [].

do_subitem(Dir, CheckResult) ->
    servant_checker_save_arched:do_subitem(Dir, CheckResult).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% ====================================================================
%% @doc Check
%% ====================================================================
is_new_archive_bigger_then_old(File) ->
    NewSize = filelib:file_size(File),
    OldFile = servant_checker_save_arched:get_new_file_name(File),
    OldSize = filelib:file_size(OldFile),
    case OldSize of
        0 -> false;   %if no old file, move only with confirmation
        _ -> NewSize > OldSize
    end.

%% ====================================================================
%% Tests
%% ====================================================================

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_new_archive_bigger_then_old_test_()->
    {
     setup,
     fun() ->
             meck:new(filelib, [unstick]),
             meck:expect(filelib, file_size,
                         fun ("basedir/dir1/dir1.rar") -> 100;
                            ("basedir/dir1.rar") -> 90;
                            ("basedir/dir2/dir2.rar") -> 100;
                            ("basedir/dir2.rar") -> 100;
                            ("basedir/dir3/dir3.rar") -> 100;
                            ("basedir/dir3.rar") -> 110;
                            ("basedir/dir4/dir4.rar") -> 100;
                            (_) -> 0 end),
             ok
     end,
     fun(_) ->
             meck:unload(filelib)
     end,
     {
      foreach,
      fun() ->
              ok
      end,
      fun(_) ->
              true = meck:validate(filelib),
              meck:reset(filelib)
      end,
      [
       ?_assertEqual({Expected, File}, {is_new_archive_bigger_then_old(File), File})
       || {Expected, File} <- 
              [
               {true, "basedir/dir1/dir1.rar"},
               {false, "basedir/dir2/dir2.rar"},
               {false, "basedir/dir3/dir3.rar"},
               {false, "basedir/dir4/dir4.rar"},
               {false, "basedir/dir5/dir5.rar"}
              ]
      ]
     }}.

get_confirmations_test_() ->
    [
     ?_assertEqual([], get_confirmations("basedir/dir1", not_used))
    ].

%-endif.
