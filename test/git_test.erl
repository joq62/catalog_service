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


-define(RepoDir,"catalog_specs_test").
-define(GitPath,"https://github.com/joq62/catalog_specs_test.git").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=setup(),
    ok=test1(),
  %  loop(false),

    io:format("Test OK !!! ~p~n",[?MODULE]),
%    timer:sleep(1000),
%    init:stop(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=catalog:update_repo_dir(?RepoDir),
    ok=catalog:update_git_path(?GitPath),
    
    %% Detect that no local repo 
    file:del_dir_r(?RepoDir),
    false=filelib:is_dir(?RepoDir),
    %% Filure test
    {error,_,_,_,_}=catalog:all_filenames(),
    {error,_,_,_,_}=catalog:read_file("first.deployment"),
    {error,_,_,_,_}=catalog:update_repo(),
    
    %and do clone 
    ok=catalog:clone(),
    true=filelib:is_dir(?RepoDir),
    {ok,[
	 "divi.application","host.application",
	 "resource_discovery.application","catalog.application",
	 "controller.application","log.application",
	 "deployment.application","adder.application"
	]
    }=catalog:all_filenames(),
    {ok,[Map]}=catalog:read_file("divi.application"),
    divi=maps:get(app,Map),
    {error,_,_,_,_}=catalog:read_file("glurk.application"),
    {error,["Already updated ",?RepoDir]}=catalog:update_repo(),
    {ok,"divi.application"}=catalog:which_filename(divi),
    {ok,["application_dir/host/ebin"]}=catalog:get_application_paths("host.application"),
    {ok,log}=catalog:get_application_app("log.application"),
    {ok,"log"}=catalog:get_application_name("log.application"),
    
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
loop(RepoState)->
  %  io:format("Start ~p~n",[{time(),?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("get all filenames ~p~n",[{catalog:all_filenames(),?MODULE,?LINE}]),
    NewState=case catalog:is_repo_updated() of
		 true->
		     case RepoState of
			 false->
			     io:format("RepoState false-> true ~p~n",[{catalog:is_repo_updated(),?MODULE,?LINE}]),
			     io:format("get all filenames ~p~n",[{catalog:all_filenames(),?MODULE,?LINE}]),
			     true;
			 true->
			     RepoState
		     end;
		 false->
		     case RepoState of
			 true->
			     io:format("RepoState true->false ~p~n",[{catalog:is_repo_updated(),?MODULE,?LINE}]),
			     io:format("catalog:update_repo()~p~n",[{catalog:update_repo(),?MODULE,?LINE}]),
			     catalog:update_repo(),
			     io:format("get all filenames ~p~n",[{catalog:all_filenames(),?MODULE,?LINE}]),
			     false;
			 false->
			     RepoState
		     end
	     end,
		    
    timer:sleep(10*1000),
    loop(NewState).

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
  
    pong=log:ping(),
    pong=rd:ping(),
    pong=git_handler:ping(),    
    pong=catalog:ping(),   
    ok.
