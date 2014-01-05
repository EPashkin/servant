%% @author Evgeny Pashkin
%% @doc Repeating task adder

-module(servant_task_starter).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("internal.hrl").

-define(TIMEOUT, 60000).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, stop/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {timeout,tasks}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
%% ====================================================================
init([]) ->
    Timeout = application:get_env(?APP, 'timeout', ?TIMEOUT),
    Tasks = application:get_env(?APP, 'tasks', []),
    case is_number(Timeout) andalso (Timeout>0) andalso is_tasks_correct(Tasks) of
        true -> {ok, #state{timeout=Timeout, tasks=Tasks}, Timeout};
        false ->
            error_logger:format("Bad parameters for servant application:~ntimeout=~p~ntasks=~p~n",
                                [Timeout, Tasks]), 
            get_error_init()
    end. 

%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
%% ====================================================================
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_error_init() ->
    {ok, #state{timeout=infinity, tasks=[]}}.


%% ====================================================================
%% @doc checks tasks format [task_dir]
%% ====================================================================
is_tasks_correct(List) when is_list(List) ->
    lists:all(fun is_task_dir_correct/1, List);
is_tasks_correct(_) ->
    false.

%% ====================================================================
%% @doc checks tasks dir format {Dir, [TaskInfo]}
%% ====================================================================
is_task_dir_correct({Dir, TaskInfos})
  when is_list(Dir) andalso is_list(TaskInfos)  ->
    lists:all(fun is_task_info_correct/1, TaskInfos);
is_task_dir_correct(_) ->
    false.

%% ====================================================================
%% @doc checks tasks dir format {CodeTag, Module}
%% ====================================================================
is_task_info_correct({CodeTag, Module})
  when is_atom(CodeTag) andalso is_atom(Module) ->
    true;
is_task_info_correct(_)->
    false.

%% ====================================================================
%% Tests
%% ====================================================================

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = start_link(),
    ?assertEqual(Pid, whereis(?MODULE)),
    ?assertEqual(ok, stop()),
    ?assertEqual(undefined, whereis(?MODULE)).

tf_tasks() ->
    [
     {true, []},
     {true, [{"", []}]},
     {false, [{"", ok}]},
     {false, [{""}]},
     {false, [{ok, []}]},
     {true, [{"", [{code, module}]}]},
     {false, [{"", [ok]}]},
     {false, [{"", [{ok}]}]},
     {false, [{"", [{ok,ok,ok}]}]},
     {false, [{"", [{ok, "ok"}]}]},
     {false, [{"", [{"ok", ok}]}]},
     {false, error}
    ].

is_tasks_correct_test_() ->
    {inparallel,
     [
      ?_assertEqual({Expected, Value}, {is_tasks_correct(Value), Value})
      || {Expected, Value} <- tf_tasks()]
    }.

init_test_() ->
    {
     foreach,
     fun() ->
             meck:new(application, [unstick, passthrough]),
             
             ok
     end,
     fun(_) ->
             true = meck:validate(application),
             meck:unload(application)
     end,
     [
      fun(_) -> %checks defaults
              meck:expect(application, get_env,
                          fun
                             (_App, _Par, Def) -> Def
                          end),
              [
               ?_assertMatch({ok,#state{timeout=?TIMEOUT, tasks=[]}, _}, init([]))
              ]
      end, 
      fun(_) -> %check timeout readed
              meck:expect(application, get_env,
                          fun
                             (_App, 'timeout', _Def) -> 10000;
                             (_App, _Par, Def) -> Def
                          end),
              [
               ?_assertMatch({ok,#state{timeout=10000}, 10000}, init([])),
               ?_assert(meck:called(application, get_env, [?APP, 'timeout', '_']))
              ]
      end, 
      fun(_) -> %check not integer timeout
              meck:expect(application, get_env,
                          fun
                             (_App, 'timeout', _Def) -> error;
                             (_App, _Par, Def) -> Def
                          end),
              [
               ?_assertEqual(get_error_init(), init([]))
              ]
      end, 
      fun(_) -> %check negative timeout
              meck:expect(application, get_env,
                          fun
                             (_App, 'timeout', _Def) -> -1;
                             (_App, _Par, Def) -> Def
                          end),
              [
               ?_assertEqual(get_error_init(), init([]))
              ]
      end, 
      fun(_) -> %check not positive timeout
              meck:expect(application, get_env,
                          fun
                             (_App, 'timeout', _Def) -> 0;
                             (_App, _Par, Def) -> Def
                          end),
              [
               ?_assertEqual(get_error_init(), init([]))
              ]
      end, 
      fun(_) -> %check tasks readed
              meck:expect(application, get_env,
                          fun
                             (_App, 'tasks', _Def) -> [{"", []}];
                             (_App, _Par, Def) -> Def
                          end),
              [
               ?_assertMatch({ok,#state{tasks=[{"",[]}]}, _}, init([])),
               ?_assert(meck:called(application, get_env, [?APP, 'tasks', '_']))
              ]
      end, 
      fun(_) -> %check bad tasks
              meck:expect(application, get_env,
                          fun
                             (_App, 'tasks', _Def) -> error;
                             (_App, _Par, Def) -> Def
                          end),
              [
               ?_assertEqual(get_error_init(), init([]))
              ]
      end
     ]
    }.

%-endif.
