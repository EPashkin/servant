%% @author Evgeny Pashkin
%% @doc Inmemory counter for new menuitem id.

-module(servant_counter).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,stop/1,getnew/0]).

start_link()->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop(Reason)->
    gen_server:call(?SERVER, {stop, Reason}).

getnew()->
    gen_server:call(?SERVER,getnew).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {nextval=1}).

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
handle_call({stop, Reason}, _From, State) ->
    {stop, Reason, ok, State};
handle_call(getnew, _From, State = #state{nextval=Next})->
    NewNext = Next + 1,
    NewState = State#state{nextval=NewNext},
    {reply, {ok, Next}, NewState};
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
    io:format("code_change: ~p~n", [OldVsn]),
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
    ok = stop(normal),
    io:format("~p~n", [whereis(?SERVER)]),
    ?assert(undefined == whereis(?SERVER)).

counter_test()->
    {ok, _Pid} = start_link(),
    ?assertEqual({ok, 1}, getnew()),
    ?assertEqual({ok, 2}, getnew()),
    ok = stop(normal),
    {ok, _Pid2} = start_link(),
    ?assertEqual({ok, 1}, getnew()),
    ok = stop(normal).

%-endif.
