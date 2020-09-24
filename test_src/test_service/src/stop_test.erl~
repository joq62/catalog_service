%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(infra_test_1).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%-compile(export_all).0
-export([start/0,stop/0]).


-define(VM_1,test_service@asus).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:emulate loader
%% Description: requires pod+container module
%% Returns: non
%% --------------------------------------------------------------------
start()->
    ?debugMsg("setup"),    
    ?assertEqual(ok,setup()),

    ?debugMsg("start_stop_service"),    
    ?assertEqual(ok,start_stop_service()),  

%    ?debugMsg("available_services_1"),    
%    ?assertEqual(ok,available_services_1()),   
%    ?debugMsg("available_services_2"),    
%    ?assertEqual(ok,available_services_2()),   
 %   ?debugMsg("stop"),    
    stop(),    
    
    ok.

%% --------------------------------------------------------------------
%% Function:emulate loader
%% Description: requires pod+container module
%% Returns: non
%% --------------------------------------------------------------------
setup()->
    ?assertMatch({pong,_,vm_service},rpc:call(?VM_1,vm_service,ping,[])),
    rpc:call(?VM_1,vm_service,stop_service,["adder_service"]),
    ok.
start_stop_service()->
    ServiceId="adder_service",
    ?assertEqual({ok,"adder_service"},
		 rpc:call(?VM_1,vm_service,start_service,[ServiceId])),
%    timer:sleep(1000),
    [Node]=sd_service:fetch_service(ServiceId),
    ?assertEqual(42,
		 rpc:call(Node,adder_service,add,[20,22])),
    ok=rpc:call(?VM_1,vm_service,stop_service,[ServiceId]),
   []=sd_service:fetch_service(ServiceId),
    ok.
    
    
stop()->
    rpc:call(?VM_1,vm_service,stop_service,["adder_service"]),
    ok.
