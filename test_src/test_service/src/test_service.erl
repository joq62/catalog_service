%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc oam  service   
%%% Central OaM functions for information 
%%%  
%%% -------------------------------------------------------------------
-module(test_service).  

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.hrl").

%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{test_suit}).


%% --------------------------------------------------------------------

-export([do_test/0,do_test/1,
	 ping/0
	]).

-export([boot/0,
	 start/0,
	 stop/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

boot()->
    ok=application:start(?MODULE).

%% Asynchrounus Signals


%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

%%-----------------------------------------------------------------------


-spec(ping()->{pong,node(),module()}).
ping()->
    gen_server:call(?MODULE, {ping},infinity).

do_test()->
    gen_server:call(?MODULE, {do_test},infinity).
do_test(TestSuit)->
    gen_server:call(?MODULE, {do_test,TestSuit},infinity).


%%-----------------------------------------------------------------------



%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
    {ok,TestSuit}= application:get_env(test_suit),
    spawn(fun()->auto_test() end),
    {ok, #state{test_suit=TestSuit}}.
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------

handle_call({do_test}, _From, State) ->
    Reply=[{M,rpc:call(node(),M,F,A)}||{M,F,A}<-State#state.test_suit],
%    Reply=[{M,apply(M,F,A)}||{M,F,A}<-State#state.test_suit],
    {reply, Reply, State};

handle_call({do_test,TestSuit}, _From, State) ->
     Reply=[{M,apply(M,F,A)}||{M,F,A}<-TestSuit],
    {reply, Reply, State};

handle_call({ping}, _From, State) ->
     Reply={pong,node(),?MODULE},
    {reply, Reply, State};


handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
auto_test()->
    timer:sleep(1000),
    io:format("~p~n",[test_service:do_test()]).
