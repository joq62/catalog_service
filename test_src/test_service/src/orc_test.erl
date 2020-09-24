%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(orc_test).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
-export([start/0]).
%-compile(export_all).



%% ====================================================================
%% External functions
%% ====================================================================

%% 
%% ----------------------------------------------- ---------------------
%% Function:emulate loader
%% Description: requires pod+container module
%% Returns: non
%% --------------------------------------------------------------------
start()->
    ?debugMsg("loop()"),
    ?assertEqual(ok,loop(2,2000,na)),
 		 
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
loop(0,_,R)->
    R;
loop(N,Interval,R)->
    
    io:format("N= ~p~n",[N]),
    timer:sleep(Interval),
    loop(N-1,Interval,R).
