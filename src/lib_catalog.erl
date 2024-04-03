%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_catalog).
  
-include("catalog.hrl").

 
%% API
-export([
	 
	 init/3,
	 update/3,
	 which_filename/2,
	 get_application_paths/3,
	 get_application_app/2,
	 get_application_name/2,
	 timer_to_call_update/1
	
	]).

-export([

	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_application_name(CatalogRepoDir,FileName)->
    Result=case rd:call(git_handler,read_file,[CatalogRepoDir,FileName],5000) of
	       {ok,[Info]}->
		   ApplicationName=maps:get(application_name,Info),
		   {ok,ApplicationName};
	       Error->
		   {error,Error}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_application_app(CatalogRepoDir,FileName)->
    Result=case rd:call(git_handler,read_file,[CatalogRepoDir,FileName],5000) of
	       {ok,[Info]}->
		   App=maps:get(app,Info),
		   {ok,App};
	       Error->
		   {error,Error}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_application_paths(CatalogRepoDir,ApplicationDir,FileName)->
    Result=case rd:call(git_handler,read_file,[CatalogRepoDir,FileName],5000) of
	       {ok,[Info]}->
		   %io:format("Info,FileName ~p~n",[{Info,FileName,?MODULE,?LINE}]),
		   RepoDir=maps:get(application_name,Info),
		   Ebin=filename:join([ApplicationDir,RepoDir,"ebin"]),
		   Priv=filename:join([ApplicationDir,RepoDir,"priv"]),
		   case filelib:is_dir(Priv) of
		       false->
			   {ok,[Ebin]};
		       true->
			   {ok,[Ebin,Priv]}
		   end;
	       Error->
		   {error,Error}
	   end,
    Result.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
which_filename(RepoDir,App)->
    {ok,AllFileNames}=rd:call(git_handler,all_filenames,[RepoDir],5000),
    find_filename(AllFileNames,RepoDir,App).

find_filename(AllFileNames,RepoDir,App)->
    find_filename(AllFileNames,RepoDir,App,false).

find_filename(_AllFileNames,_RepoDir,_App,{ok,FileName})->
    {ok,FileName};
find_filename([],_RepoDir,_App,Found)->
    Found;
find_filename([FileName|T],RepoDir,App,false)->
    {ok,[Map]}=rd:call(git_handler,read_file,[RepoDir,FileName],5000),
    NewAcc=case maps:get(app,Map) of
	       App->
		   {ok,FileName};
	       _ ->
		   false
	   end,
    find_filename(T,RepoDir,App,NewAcc).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
timer_to_call_update(Interval)->
   % io:format(" ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    timer:sleep(Interval),
    rpc:cast(node(),catalog,update,[]).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
update(RepoDir,GitPath,ApplicationDir)->
    io:format(" ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    case rd:call(git_handler,is_repo_updated,[RepoDir],5000) of
	{error,["RepoDir doesnt exists, need to clone"]}->
	    ok=rd:call(git_handler,clone,[RepoDir,GitPath],5000);
	false ->
	    io:format(" ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
	    ok=rd:call(git_handler,update_repo,[RepoDir],5000);
	true ->
	    ok
    end,
    case filelib:is_dir(ApplicationDir) of
	false->
	    ok=file:make_dir(ApplicationDir);
	true->
	    ok
    end,
    {ok,AllFileNames}=rd:call(git_handler,all_filenames,[RepoDir],5000),
    R=[{update_application(FileName,RepoDir,ApplicationDir),FileName}||FileName<-AllFileNames],
    []=[{X,FileName}||{X,FileName}<-R,
		      ok=/=X],

    ok.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
init(RepoDir,GitPath,ApplicationDir)->
    file:del_dir_r(RepoDir),
    ok=rd:call(git_handler,clone,[RepoDir,GitPath],5000),
    file:del_dir_r(ApplicationDir),
    file:make_dir(ApplicationDir),
    {ok,AllFileNames}=rd:call(git_handler,all_filenames,[RepoDir],5000),
    R=[{update_application(FileName,RepoDir,ApplicationDir),FileName}||FileName<-AllFileNames],
    []=[{X,FileName}||{X,FileName}<-R,
		      ok=/=X],
    ok.

	       


%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
update_application(FileName,CatalogRepoDir,ApplicationDir)->
    Result=case rd:call(git_handler,read_file,[CatalogRepoDir,FileName],5000) of
	       {ok,[Info]}->
		   %io:format("Info,FileName ~p~n",[{Info,FileName,?MODULE,?LINE}]),
		   RepoDir=maps:get(application_name,Info),
		   GitPath=maps:get(git,Info),
		   FullRepoDir=filename:join([ApplicationDir,RepoDir]),
		   case rd:call(git_handler,is_repo_updated,[FullRepoDir],5000) of
		       {error,["RepoDir doesnt exists, need to clone"]}->
			  rd:call(git_handler,clone,[FullRepoDir,GitPath],5000);
		       false ->
			  rd:call(git_handler,update_repo,[FullRepoDir],5000);
		       true ->
			   ok
		   end;
	       Error->
		   {error,Error}
	  end,
    Result.
