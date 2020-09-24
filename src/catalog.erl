%%% -------------------------------------------------------------------
%%% @author : joqerlang
%%% @doc : ets dbase for master service to manage app info , catalog  
%%%
%%% -------------------------------------------------------------------
-module(catalog). 
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%-compile(export_all).
-export([service_list/0,
	 create/1,
	 delete/1,
	 update/1,
	 get/1]).

%% --------------------------------------------------------------------
%% Function: service_list()
%% Description: Returns list of all applications  
%% Returns: {ok,[{ApplicationId,Vsn}]} |{error,Err}
%
%% --------------------------------------------------------------------
service_list()->
    catalog_service:service_list().

%% --------------------------------------------------------------------
%% Function: get(ApplicationId,Vsn)
%% Description: Returns stored DeploymentSpec for AppId  
%% Returns: {ok,#deployment_spec} |{error,Err}
%% --------------------------------------------------------------------

get(ApplicationId,Vsn)->
    catalog_service:create(ApplicationId,Vsn).

%% --------------------------------------------------------------------
%% Function: create(DeploymentSpec)
%% Description: Checks and stores DeploymentSpec 
%% Returns: ok |{error,Err}
%% --------------------------------------------------------------------

create(DeploymentSpec)->
    catalog_service:create(DeploymentSpec).

%% --------------------------------------------------------------------
%% Function: create(DeploymentSpec)
%% Description: Checks and stores DeploymentSpec 
%% Returns: ok |{error,Err}
%% --------------------------------------------------------------------

create(DeploymentSpec)->
    catalog_service:create(DeploymentSpec).

%% --------------------------------------------------------------------
%% Function: delete(DeploymentSpec)
%% Description: Deletes DeploymentSpec 
%% Returns: ok |{error,Err}
%% --------------------------------------------------------------------

delete(DeploymentSpec)->
    catalog_service:delete(DeploymentSpec).

%% --------------------------------------------------------------------
%% Function: update(UpdatedDeploymentSpec)
%% Description: Update an existing DeploymentSpec 
%% Returns: ok |{error,Err}
%% --------------------------------------------------------------------

update(UpdatedDeploymentSpec)->
    catalog_service:update(UpdatedDeploymentSpec).
