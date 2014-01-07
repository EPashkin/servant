%% @author Evgeny Pashkin
%% @doc Worker task queue


-module(servant_task_queue_manager).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("internal.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, stop/0]).
-export([
         len/0,
         state/0,
         in/1,
         in_after/2,
         get_task/1
        ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

len() ->
    gen_server:call(?MODULE, len).

state() ->
    gen_server:call(?MODULE, state).

in(Task) when is_record(Task, task) ->
    in_internal(Task).

in_internal(Task) ->
    gen_server:cast(?MODULE, {in, Task}).

in_after(Time, Task) when is_integer(Time), Time >= 0 ->
    timer:apply_after(Time, ?MODULE, in, [Task]).

get_task(WorkerPid) ->
    gen_server:cast(?MODULE, {get_task, WorkerPid}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {
                queue = queue:new() :: queue(),
                waiting_workers = [] :: [pid()],
                monitors = dict:new() :: dict(),
                queue_length = 0 :: non_neg_integer() % queue:len() is O(N)
               }).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
%% ====================================================================
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
%% ====================================================================
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(len, _From, #state{ queue_length = QueueLength } = State) ->
    {reply, QueueLength, State};
handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
%% ====================================================================
handle_cast({get_task, WorkerPid}, #state{ queue_length = 0 } = State) ->
    {noreply, add_waiting_worker(WorkerPid, State)};
handle_cast({get_task, WorkerPid}, State) ->
    {Task, NewState} = dequeue_task(State),
    WorkerPid ! { task, Task },
    {noreply, NewState};
handle_cast({in, Task}, State) ->
    NewState =
        case there_are_waiting_workers(State) of
            true ->
                Worker = get_first_waiting_worker(State),
                Worker ! { task, Task},
                del_first_waiting_worker(State);
            false ->
                enqueue_task(Task, State)
        end,
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
%% ====================================================================
handle_info({'DOWN', _MonitorRef, _Type, Pid, _Info}, #state{} = State) ->
    {noreply, del_died_waiting_worker(Pid, State)};
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

enqueue_task(Task,
             #state{
                    queue = Queue,
                    queue_length = QueueLength } = State) ->
    State#state{
                queue = queue:in(Task, Queue),
                queue_length = QueueLength + 1
               }.


dequeue_task(#state{ queue = Queue, queue_length = QueueLength } = State) ->
    {{value, Task}, NewQueue} = queue:out(Queue),
    NewState = State#state{
                           queue = NewQueue,
                           queue_length = QueueLength - 1
                          },
    { Task, NewState }.

there_are_waiting_workers(#state{ waiting_workers = []}) ->
    false;

there_are_waiting_workers(#state{ waiting_workers = [ _ | _ ]}) ->
    true.

get_first_waiting_worker(#state{ waiting_workers = [ Worker | _ ]}) ->
    Worker.

add_waiting_worker(Worker,
                   #state{ monitors = MonitorDict, waiting_workers = WaitingWorkers } = State) ->
    MonitorRef = erlang:monitor(process, Worker),
    State#state{
                monitors = dict:store(Worker, MonitorRef, MonitorDict),
                waiting_workers = [Worker | WaitingWorkers ]
               }.

del_first_waiting_worker(
  #state{
         monitors = MonitorDict,
         waiting_workers = [ Worker | WaitingWorkersTail ]
        } = State) ->
    {ok, MonitorRef} = dict:find(Worker, MonitorDict),
    erlang:demonitor(MonitorRef),
    State#state{
                monitors = dict:erase(Worker, MonitorDict),
                waiting_workers = WaitingWorkersTail
               }.

del_died_waiting_worker(Worker,
                        #state{
                               monitors = MonitorDict,
                               waiting_workers = WaitingWorkers
                              } = State) ->
    State#state{
                monitors = dict:erase(Worker, MonitorDict),
                waiting_workers = WaitingWorkers -- [Worker]
               }.

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

in_len_test_() ->
    {
     foreach,
     fun() ->
             {ok, _Pid} = start_link()
     end,
     fun(_) -> 
             ok = stop()
     end,
     [
      fun(_) -> [
                 ?_assertEqual(0, len()),
                 ?_assertEqual(ok, in_internal(code1)),
                 ?_assertEqual(1, len()),
                 ?_assertEqual(ok, in_internal(code1)),
                 ?_assertEqual(2, len()),
                 ?_assertMatch(#state{queue_length=2}, state())
                ]
      end
     ]
    }.

get_task_test_() ->
    {
     foreach,
     fun() ->
             {ok, _Pid} = start_link()
     end,
     fun(_) -> 
             ok = stop()
     end,
     [
      %not empty queue test
      fun(_) -> [
                 ?_assertEqual(0, len()),
                 fun() ->
                         TestPid = self(),
                         Func = fun () ->
                                         ok = get_task(self()),
                                         receive
                                             {task, Task} -> TestPid ! {ok, Task}
                                         after
                                                 50 -> TestPid ! timeout
                                         end
                                end,
                         in_internal(code1),
                         _Pid = spawn_link(Func),
                         Result = receive
                                      Msg -> Msg 
                                  after
                                          1000 -> no_result
                                  end,
                         ?assertEqual({ok, code1}, Result)
                 end,
                 ?_assertEqual(0, len())
                ]
      end,
      %add after request test
      fun(_) -> [
                 ?_assertEqual(0, len()),
                 fun() ->
                         TestPid = self(),
                         Func = fun () ->
                                         receive
                                             {task, Task} -> TestPid ! {ok, Task}
                                         after
                                                 50 -> TestPid ! timeout
                                         end
                                end,
                         Pid = spawn_link(Func),
                         ok = get_task(Pid),
                         in_internal(code1),
                         ?assertEqual(0, len()),
                         Result = receive
                                      Msg -> Msg 
                                  after
                                          1000 -> no_result
                                  end,
                         ?assertEqual({ok, code1}, Result)
                 end
                ]
      end,
      %died worker test
      fun(_) -> [
                 ?_assertEqual(0, len()),
                 fun() ->
                         TestPid = self(),
                         Func = fun () ->
                                         ok = get_task(self()),
                                         TestPid ! exiting
                                end,
                         _Pid = spawn(Func),
                         Result = receive
                                      Msg -> Msg 
                                  after
                                          1000 -> no_result
                                  end,
                         in_internal(code1),
                         ?assertEqual(1, len()),
                         ?assertEqual(exiting, Result)
                 end
                ]
      end
     ]
    }.

%-endif.
