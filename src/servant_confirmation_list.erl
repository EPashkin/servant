%% @author Evgeny Pashkin
%% @doc List of possible tasks.

-module(servant_confirmation_list).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).

-include("internal.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, stop/0]).
-export([
         add_confirmation/1,
         add_confirmation/3,
         get_confirmations/0,
         process_confirmation/1
        ]).

start_link()->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop()->
    gen_server:call(?SERVER, stop).

add_confirmation(Text, Code, Module) ->
    add_confirmation(#confirmation{text=Text, code=Code, module=Module}).
add_confirmation(#confirmation{}=Confirmation) ->
    gen_server:call(?SERVER, {add_confirmation, Confirmation}).

get_confirmations() ->
    gen_server:call(?SERVER, get_confirmations).

process_confirmation(Code) ->
    gen_server:call(?SERVER, {process_confirmation, Code}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {list=[]}).

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
handle_call({add_confirmation, #confirmation{code=Code}=Confirmation}, _From, State=#state{list=List}) ->
    Pred = fun (#confirmation{code=ItemCode}) -> ItemCode == Code end,
    NewList = case lists:any(Pred, List) of
                  true -> List;
                  false-> [Confirmation|List]
              end,
    {reply, ok, State#state{list=NewList}};
handle_call(get_confirmations, _From, State=#state{list=List}) ->
    ReplyList = [{Confirmation#confirmation.text, Confirmation#confirmation.code}
                 || Confirmation <- List],
    {reply, {ok, lists:reverse(ReplyList)}, State};
handle_call({process_confirmation, Code}, _From, State=#state{list=List}) ->
    {Reply, NewState} = case find_by_code(Code, List) of
                            false -> {unknown_code, State};
                            #confirmation{}=Confirmation -> 
                                Task = confirmation_to_task(Confirmation),
                                servant_task_queue_manager:in(Task), 
                                {ok, State#state{list=delete_by_code(Code, List)}}    
                        end,
    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
    Reply = {error, bad_request},
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
code_change(OldVsn, State, _Extra) ->
    io:format("~p code_change: ~p~n", [?MODULE, OldVsn]),
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
find_by_code(Code, List) ->
    lists:keyfind(Code, #confirmation.code, List).
delete_by_code(Code, List) ->
    lists:keydelete(Code, #confirmation.code, List).

confirmation_to_task(#confirmation{code=Code, module=Module}) ->
    #task{code=Code, module=Module}.

%% ====================================================================
%% Tests
%% ====================================================================

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = start_link(),
    ?assert(Pid == whereis(?SERVER)),
    ok = stop(),
    io:format("~p~n", [whereis(?SERVER)]),
    ?assert(undefined == whereis(?SERVER)).

add_test_() ->
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
                 ?_assertEqual(ok, add_confirmation("Text1", code1, mod)),
                 ?_assertEqual({ok, [{"Text1", code1}]}, get_confirmations()),              
                 ?_assertEqual(ok, add_confirmation("Text2", code2, mod)),
                 ?_assertEqual({ok, [{"Text1", code1},
                                     {"Text2", code2}]}, get_confirmations())
                ]
      end,
      %check not add items with same code
      fun(_) -> [
                 ?_assertEqual(ok, add_confirmation("Text1", code1, mod)),
                 ?_assertEqual(ok, add_confirmation("Text2", code1, mod)),
                 ?_assertEqual({ok, [{"Text1", code1}]}, get_confirmations()),
                 %check that list not reordered
                 ?_assertEqual(ok, add_confirmation("Text3", code3, mod)),
                 ?_assertEqual(ok, add_confirmation("Text4", code1, mod)),
                 ?_assertEqual({ok, [{"Text1", code1},
                                     {"Text3", code3}]}, get_confirmations())
                ]              
      end,
      fun(_)->[
               ?_assertEqual(unknown_code, process_confirmation(code1)),
               ?_assertEqual(ok, add_confirmation("Text1", code1, mod)),
               fun() ->
                       meck:new(servant_task_queue_manager),
                       meck:expect(servant_task_queue_manager, in, 1, ok),
                       
                       ?assertEqual(ok, process_confirmation(code1)),
                       ?assertEqual({ok,[]}, get_confirmations()), %check remove from list                       
                       ?assert(meck:called(servant_task_queue_manager, in, [#task{code=code1, module=mod}])),
                       
                       %cleanup
                       true = meck:validate(servant_task_queue_manager),
                       meck:unload(servant_task_queue_manager)
               end
              ]
      end
     ]
    }.

%-endif.
