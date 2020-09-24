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


-ifdef(dir).
-define(CHECK_CATALOG,check_catalog_dir()).
-else.
-define(CHECK_CATALOG,check_catalog_git()).
-endif.


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
    ?assertEqual(ok,application:start(orchistrate_service)),
    ?debugMsg("get_all"),
    ?assertEqual(ok,get_all()),
    ?debugMsg("get_service"),
    ?assertEqual(ok,get_service()),

    ?debugMsg("update_catalog"),
    ?assertEqual(ok,update_info()),
    ?assertEqual(ok,application:stop(orchistrate_service)),		 
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
get_all()->
    ?assertEqual([{"iaas_service",master_sthlm_1@asus},
		  {"adder_service",worker_sthlm_3@asus},
		  {"adder_service",worker_sthlm_1@asus},
		  {"divi_service",worker_sthlm_2@asus},
		  {"orchistrate_service",master_sthlm_1@asus}],orchistrate_service:get_info(all)),
    ok.

get_service()->
    ?assertEqual([{"adder_service",worker_sthlm_3@asus},
		  {"adder_service",worker_sthlm_1@asus}],orchistrate_service:get_info("adder_service")),
    ok.

update_info()->
    ?assertEqual(ok,orchistrate_service:update_info()),
    ok.
