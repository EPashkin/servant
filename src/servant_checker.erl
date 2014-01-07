%% @author Evgeny Pashkin
%% @doc Generic checker caller

-module(servant_checker).

-include("internal.hrl").

-export([behaviour_info/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([process_task/1, check_task/1]).

behaviour_info(callbacks) ->
    [
     {get_subitems, 1},
     {check_subitem, 1},
     {get_confirmations, 2},
     {do_subitem, 2}
    ];
behaviour_info(_Other) ->
    undefined.

process_task({Code, Dir}) when is_atom(Code) ->
    case analyze_code(Code) of
        {Oper, Module} -> do_task(Oper, Dir, Module);
        _ -> error_logger:format("Wrong code in ~p:process_task(~p)~n", [?MODULE, Code])
    end,
    ok;
process_task(Code) ->
    error_logger:format("Unknown code in ~p:process_task(~p)~n", [?MODULE, Code]),
    ok.

check_task({Code, Dir}) when is_atom(Code) ->
    case analyze_code(Code) of
        {Oper, Module} -> do_check(Oper, Dir, Module);
        _ -> false
    end;
check_task(Code) ->
    error_logger:format("Unknown code in ~p:check_task(~p)~n", [?MODULE, Code]),
    false.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% ====================================================================
%% @doc Do task 
%% ====================================================================
do_task(check, Dir,  Module) ->
    SubDirs = Module:get_subitems(Dir),
    Func = fun(SubDir) ->
                   case Module:check_subitem(SubDir) of
                       false -> ok;
                       CheckResult -> Confirmations = Module:get_confirmations(SubDir, CheckResult),
                                      add_confirmations(Confirmations)
                   end
           end,
    lists:foreach(Func, SubDirs),
    ok;
do_task(do, Dir, Module) ->
    case Module:check_subitem(Dir) of
        false -> ok;
        CheckResult -> Module:do_subitem(Dir, CheckResult)
    end.

%% ====================================================================
%% @doc Check task is actual 
%% ====================================================================
do_check(check, _Dir, _Module) ->
    true;
do_check(do, Dir, Module) ->
    case Module:check_subitem(Dir) of
        false -> false;
        _ -> true
    end.

%% ====================================================================
%% @doc Extract from code operation and module
%% check_save_arched -> {check, servant_checker_save_arched}. 
%% ====================================================================
analyze_code(Code) when is_atom(Code) ->
    CodeString = atom_to_list(Code),
    case CodeString of
        [$c,$h,$e,$c,$k,$_ | Mod] -> process_oper_module(check, Mod);
        [$d,$o,$_ | Mod] -> process_oper_module(do, Mod);
        _ -> Code
    end.

%% ====================================================================
%% @doc 
%% check_save_arched -> {check, servant_checker_save_arched}. 
%% ====================================================================
process_oper_module(Oper, Mod)
  when is_atom(Oper) andalso is_list(Mod)->
    Module = list_to_atom("servant_checker_" ++ Mod),
    case module_exists(Module) of
        true -> {Oper, Module};
        false -> Module
    end.

%% ====================================================================
%% @doc Test whether a module exists
%% @spec module_exists(module()) -> boolean()
%% ====================================================================
module_exists(Module) ->
    case is_atom(Module) of
        true ->
            try Module:module_info() of
                _InfoList ->
                    true
            catch
                _:_ ->
                    false
            end;
        
        false ->
            false
    end.

%% ====================================================================
%% @doc Add confirmations from list
%% ====================================================================
add_confirmations([])->
    ok;
add_confirmations([#confirmation{text=IOList}=Confirmation | Rest])->
    Text = lists:flatten(IOList),
    servant_confirmation_list:add_confirmation(Confirmation#confirmation{text=Text}),
    add_confirmations(Rest).

%% ====================================================================
%% Tests
%% ====================================================================

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_confirmations_test_() ->
    {
     foreach,
     fun() ->
             meck:new(servant_confirmation_list),
             meck:expect(servant_confirmation_list, add_confirmation, 1, ok),
             ok
     end,
     fun(_) ->
             true = meck:validate(servant_confirmation_list),
             meck:unload(servant_confirmation_list)
     end,
     [
      fun(_) ->
              [
               ?_assertEqual(ok, add_confirmations([])),
               ?_assertNot(meck:called(servant_confirmation_list, add_confirmation, ['_', '_', '_']))
              ]
      end,
      fun(_) ->
              Confirmation = #confirmation{text=["Text"], code=code1, module=module1}, 
              [
               ?_assertEqual(ok, add_confirmations([Confirmation])),
               ?_assert(meck:called(servant_confirmation_list, add_confirmation, ['_'])),
               ?_assert(meck:called(servant_confirmation_list, add_confirmation, 
                                    [Confirmation#confirmation{text="Text"}]))
              ]
      end
     ]}.

functions_test_() ->
    {
     foreach,
     fun() ->
             meck:new(servant_checker_test, [non_strict]),
             ok
     end,
     fun(_) ->
             true = meck:validate(servant_checker_test),
             meck:unload(servant_checker_test)
     end,
     [
      fun(_) -> %module_exists
              [
               ?_assertEqual(true, module_exists(servant_checker_test)),
               ?_assertEqual(false, module_exists(servant_checker_test1)),
               ?_assertEqual(false, module_exists("servant_checker_test"))
              ]
      end,
      fun(_) -> %process_oper_module
              [
               ?_assertEqual({check, servant_checker_test}, process_oper_module(check, "test")),
               ?_assertNotMatch({check, _}, process_oper_module(check, "test1"))
              ]
      end,
      fun(_) -> %analyze_code
              [
               ?_assertEqual({check, servant_checker_test}, analyze_code(check_test)),
               ?_assertEqual({do, servant_checker_test}, analyze_code(do_test)),
               ?_assertEqual(do1_test, analyze_code(do1_test)),
               ?_assertEqual(servant_checker_test1, analyze_code(do_test1))
              ]
      end,
      fun(_) -> %do_task(check)
              meck:expect(servant_checker_test, get_subitems, 1, ["basedir/dir1", "basedir/dir2"]),
              meck:expect(servant_checker_test, check_subitem,
                          fun ("basedir/dir1") -> "basedir/dir1/dir1.rar"; 
                             ("basedir/dir2") -> false
                          end),
              meck:expect(servant_checker_test, get_confirmations, 2, []),
              [
               ?_assertEqual(ok, do_task(check, "basedir", servant_checker_test)),
               ?_assert(meck:called(servant_checker_test, get_subitems, ["basedir"])),
               ?_assert(meck:called(servant_checker_test, check_subitem, ["basedir/dir1"])),
               ?_assert(meck:called(servant_checker_test, check_subitem, ["basedir/dir2"])),
               ?_assert(meck:called(servant_checker_test, get_confirmations, ["basedir/dir1", "basedir/dir1/dir1.rar"]))
              ]
      end,
      fun(_) -> %do_task(do)
              meck:expect(servant_checker_test, check_subitem,
                          fun ("basedir/dir1") -> "basedir/dir1/dir1.rar"; 
                             ("basedir/dir2") -> false
                          end),
              meck:expect(servant_checker_test, get_confirmations, 2, []),
              meck:expect(servant_checker_test, do_subitem, 2, test),
              [
               %with good check
               ?_assertEqual(test, do_task(do, "basedir/dir1", servant_checker_test)),
               ?_assert(meck:called(servant_checker_test, check_subitem, ["basedir/dir1"])),
               ?_assert(meck:called(servant_checker_test, do_subitem, ["basedir/dir1", "basedir/dir1/dir1.rar"])),
               %with bad check
               ?_assertEqual(ok, do_task(do, "basedir/dir2", servant_checker_test)),
               ?_assert(meck:called(servant_checker_test, check_subitem, ["basedir/dir2"])),
               ?_assertNot(meck:called(servant_checker_test, do_subitem, ["basedir/dir2", '_']))
              ]
      end,
      fun(_) -> %do_check(check)
              meck:expect(servant_checker_test, check_subitem,
                          fun ("basedir/dir1") -> "basedir/dir1/dir1.rar"; 
                             ("basedir/dir2") -> false
                          end),
              meck:expect(servant_checker_test, get_confirmations, 2, []),
              meck:expect(servant_checker_test, do_subitem, 2, test),
              [
               ?_assertEqual(true, do_check(check, "", servant_checker_test)),
               ?_assertNot(meck:called(servant_checker_test, check_subitem, ['_']))
              ]
      end,
      fun(_) -> %do_check(do)
              meck:expect(servant_checker_test, check_subitem,
                          fun ("basedir/dir1") -> "basedir/dir1/dir1.rar"; 
                             ("basedir/dir2") -> false
                          end),
              [
               %with good check
               ?_assertEqual(true, do_check(do, "basedir/dir1", servant_checker_test)),
               ?_assert(meck:called(servant_checker_test, check_subitem, ["basedir/dir1"])),
               %with bad check
               ?_assertEqual(false, do_check(do, "basedir/dir2", servant_checker_test)),
               ?_assert(meck:called(servant_checker_test, check_subitem, ["basedir/dir2"]))
              ]
      end,
      fun(_) -> %check_task
              meck:expect(servant_checker_test, check_subitem,
                          fun ("basedir/dir1") -> "basedir/dir1/dir1.rar"; 
                             ("basedir/dir2") -> false
                          end),
              [
               %with good check
               ?_assertEqual(true, check_task({do_test, "basedir/dir1"})),
               ?_assert(meck:called(servant_checker_test, check_subitem, ["basedir/dir1"])),
               %with bad oper check
               ?_assertEqual(false, check_task({doo_test, "basedir/dir2"})),
               ?_assertNot(meck:called(servant_checker_test, check_subitem, ["basedir/dir2"])),
               %with bad args check
               ?_assertEqual(false, check_task(do_test)),
               ?_assertNot(meck:called(servant_checker_test, check_subitem, ["basedir/dir2"]))
              ]
      end
     ]
    }.

%-endif.
