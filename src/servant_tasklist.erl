%% @author Evgeny Pashkin
%% @doc List of possible tasks.

-module(servant_tasklist).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, stop/0]).
-export([addtask/2, getForMenu/0]).

start_link()->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop()->
    gen_server:call(?SERVER, stop).

addtask(Text,Code) ->
    gen_server:call(?SERVER, {addtask, Text, Code}).

getForMenu() ->
    gen_server:call(?SERVER, getForMenu).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {list=[]}).
-record(taskinfo,{text,code}).

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
handle_call({addtask, Text, Code}, _From, State=#state{list=List}) ->
    Pred = fun (#taskinfo{code=ItemCode}) -> ItemCode == Code end,
    NewList = case lists:any(Pred, List) of
                  true -> List;
                  false->Info = #taskinfo{text=Text, code=Code},
                         [Info|List]
              end,
    {reply, ok, State#state{list=NewList}};
handle_call(getForMenu, _From, State=#state{list=List}) ->
    ReplyList = [{TaskInfo#taskinfo.text, TaskInfo#taskinfo.code}
                 || TaskInfo <- List],
    {reply, {ok, lists:reverse(ReplyList)}, State};
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
                 ?_assertEqual(ok, addtask("Text1", code1)),
                 ?_assertEqual({ok, [{"Text1", code1}]}, getForMenu()),              
                 ?_assertEqual(ok, addtask("Text2", code2)),
                 ?_assertEqual({ok, [{"Text1", code1},
                                     {"Text2", code2}]}, getForMenu())
                ]
      end,
      %check not add items with same code
      fun(_) -> [
                 ?_assertEqual(ok, addtask("Text1", code1)),
                 ?_assertEqual(ok, addtask("Text2", code1)),
                 ?_assertEqual({ok, [{"Text1", code1}]}, getForMenu()),
                 %check that list not reordered
                 ?_assertEqual(ok, addtask("Text3", code3)),
                 ?_assertEqual(ok, addtask("Text4", code1)),
                 ?_assertEqual({ok, [{"Text1", code1},
                                     {"Text3", code3}]}, getForMenu())
                ]              
      end
     ]
    }.

%-endif.
