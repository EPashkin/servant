%% @author Evgeny Pashkin
%% @doc Worker for task queue

-module(servant_task_queue_worker).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("internal.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
%% ====================================================================
init([]) ->
    self() ! init,
    {ok, not_inited}.

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
handle_info(init, not_inited) ->
    servant_task_queue_manager:get_task(self()),
    {noreply, #state{}};
handle_info({task, #task{code=Code, module=Module}=Task}, State) ->
    Reply = erlang:apply(Module, process_task, [Code]),
    case Reply of
        {timeout, Time} ->
            servant_task_queue_manager:in_after(Time, Task);
        ok -> true
    end,
    servant_task_queue_manager:get_task(self()),
    {noreply, State};
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


%% ====================================================================
%% Tests
%% ====================================================================

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = start_link(),
    ?assertEqual(true, is_process_alive(Pid)),
    ?assertEqual(ok, stop(Pid)),
    ?assertEqual(false, is_process_alive(Pid)).

task_test_() ->
    {timeout, 2,
    fun()->
    meck:new(servant_task_queue_manager),
    meck:new(test_worker, [non_strict]),
    meck:expect(servant_task_queue_manager, get_task,
                fun (WorkerPid) ->
                         WorkerPid!{task, #task{code=code1, module=test_worker}}
                end),
    TestPid = self(),
    meck:expect(test_worker, process_task,
                fun(Code)->
                        %removing expectation for disable recursion
                        meck:expect(servant_task_queue_manager, get_task,
                                    fun (_WorkerPid) -> ok end),
                        TestPid ! {code, Code},
                        ok
                end),
    
    {ok, Pid} = start_link(),
    Result = receive
                 {code, Code} -> {ok, Code}
             end,
    ?assertEqual({ok, code1}, Result),
    
    %cleanup
    ok = stop(Pid),
    true = meck:validate(test_worker),
    meck:unload(test_worker),
    true = meck:validate(servant_task_queue_manager),
    meck:unload(servant_task_queue_manager),
    ?_assert(true)
    end}.

task_timeout_test_() ->
    {timeout, 2,
    fun()->
    meck:new(servant_task_queue_manager),
    meck:new(test_worker, [non_strict]),
    meck:expect(servant_task_queue_manager, get_task,
                fun (WorkerPid) ->
                         WorkerPid!{task, #task{code=code1, module=test_worker}}
                end),
    TestPid = self(),
    meck:expect(servant_task_queue_manager, in_after,
                fun (Time, Task) ->
                         TestPid ! {delaying, Time, Task}
                end),
    meck:expect(test_worker, process_task,
                fun(_Code)->
                        %removing expectation for disable recursion
                        meck:expect(servant_task_queue_manager, get_task,
                                    fun (_WorkerPid) -> ok end),
                        {timeout, 100}
                end),
    
    {ok, Pid} = start_link(),
    Result = receive
                 {delaying, _Time, Task} -> {ok, Task}
             end,
    ?assertMatch({ok, #task{code=code1}}, Result),
    
    %cleanup
    ok = stop(Pid),
    true = meck:validate(test_worker),
    meck:unload(test_worker),
    true = meck:validate(servant_task_queue_manager),
    meck:unload(servant_task_queue_manager),
    ?_assert(true)
    end}.

%-endif.
