%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(git_test).      
 
-export([start/0]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("catalog.hrl").

-define(TestApplicationId,"log").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=setup(),
    ok=git_repo(),
    ok=git_application(),
    ok=start_application(),
 
    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(1000),
    init:stop(),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_application()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    {ok,Paths}=catalog:get_application_paths(?TestApplicationId),
    ["catalog/application_dir/log/ebin"]=Paths,
    {ok,App}=catalog:get_application_app(?TestApplicationId),
    log=App,
    
    HostId=net_adm:localhost(),
    NodeName="n1",
    Cookie=atom_to_list(erlang:get_cookie()),
    Args=" -setcookie "++Cookie,
    {ok,N1}=slave:start(HostId,NodeName,Args),
    [true]=[rpc:call(N1,code,add_patha,[Path],5000)||Path<-Paths],
    ok=rpc:call(N1,application,load,[App],5000),
    ok=rpc:call(N1,application,start,[App],5000),
    pong=rpc:call(N1,log,ping,[],5000),
    
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
git_application()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    [
     "adder","controller","deployment",
     "divi","host","log","resource_discovery","worker"
    ]=lists:sort(catalog:get_all_ids()),
    
    %% check if updated
  [
   {error,["Applications local repo doesnt exists","catalog/application_dir/adder"]},
   {error,["Applications local repo doesnt exists","catalog/application_dir/controller"]},
   {error,["Applications local repo doesnt exists","catalog/application_dir/deployment"]},
   {error,["Applications local repo doesnt exists","catalog/application_dir/divi"]},
   {error,["Applications local repo doesnt exists","catalog/application_dir/host"]},
   {error,["Applications local repo doesnt exists","catalog/application_dir/log"]},
   {error,["Applications local repo doesnt exists","catalog/application_dir/resource_discovery"]},
   {error,["Applications local repo doesnt exists","catalog/application_dir/worker"]}
  ]=[catalog:is_application_repo_updated(ApplicationId)||ApplicationId<-lists:sort(catalog:get_all_ids())],

    [ok,ok,ok,ok,ok,ok,ok,{error,_}]=[catalog:clone_application_repo(ApplicationId)||ApplicationId<-lists:sort(catalog:get_all_ids())],
    
  

   % [
   %  {app,log},{application_name,"log"},{erl_args," "},
   %  {git,"https://github.com/joq62/log.git"},
   %  {id,"log"},{vsn,"0.1.0"}
   % ]=lists:sort(maps:to_list(Map)),

    {ok,?TestApplicationId}=catalog:get_info(id,?TestApplicationId),
    {ok,"log"}=catalog:get_info(application_name,?TestApplicationId),
    {ok,"0.1.0"}=catalog:get_info(vsn,?TestApplicationId),
    {ok,log}=catalog:get_info(app,?TestApplicationId),
    {ok," "}=catalog:get_info(erl_args,?TestApplicationId),
    {ok,"https://github.com/joq62/log.git"}=catalog:get_info(git,?TestApplicationId),

    {error,{badkey,glurk},_,_,_}=catalog:get_info(glurk,?TestApplicationId),
   {error,["ApplicationId doens't exists",glurk]}=catalog:get_info(git,glurk),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
git_repo()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {error,["RepoDir doesnt exists, need to clone","catalog/catalog_specs"]}=catalog:is_repo_updated(),
    ok=catalog:clone_repo(),
    true=catalog:is_repo_updated(),
    {error,["Already updated ","catalog/catalog_specs"]}=catalog:update_repo(),
    
   [
    "adder","controller","deployment",
    "divi","host","log","resource_discovery","worker"
   ]=lists:sort(catalog:get_all_ids()),
   
    {ok,Map}=catalog:get_map(?TestApplicationId),
   
    [
     {app,log},{application_name,"log"},{erl_args," "},
     {git,"https://github.com/joq62/log.git"},
     {id,"log"},{vsn,"0.1.0"}
    ]=lists:sort(maps:to_list(Map)),

    {ok,?TestApplicationId}=catalog:get_info(id,?TestApplicationId),
    {ok,"log"}=catalog:get_info(application_name,?TestApplicationId),
    {ok,"0.1.0"}=catalog:get_info(vsn,?TestApplicationId),
    {ok,log}=catalog:get_info(app,?TestApplicationId),
    {ok," "}=catalog:get_info(erl_args,?TestApplicationId),
    {ok,"https://github.com/joq62/log.git"}=catalog:get_info(git,?TestApplicationId),

    {error,{badkey,glurk},_,_,_}=catalog:get_info(glurk,?TestApplicationId),
   {error,["ApplicationId doens't exists",glurk]}=catalog:get_info(git,glurk),
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    file:del_dir_r(?RepoDir),
    ok.
