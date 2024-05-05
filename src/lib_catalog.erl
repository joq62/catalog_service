%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_catalog).
  
-include("log.api").
-include("catalog.hrl").

 
%% API
-export([
	 start/3,
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

start(LocalRepoDir,GitPath,LocalApplicationDir)->
  %  io:format(" START Reconcilaition ******************** ~p~n",[{??MODULE,?FUNCTION_NAME,?LINE}]),
    timer:sleep(?ReconciliationInterval),
    {ok,CurrentDir}=file:get_cwd(),
    RepoDir=filename:join([CurrentDir,LocalRepoDir]),
    ApplicationDir=filename:join([CurrentDir,LocalApplicationDir]),
    update(RepoDir,GitPath,ApplicationDir),
  %  io:format(" END Reconcilaition ========================== ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    rpc:cast(node(),catalog,reconciliate,[]).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_application_name(CatalogRepoDir,FileName)->
    Result=case git_handler:read_file(CatalogRepoDir,FileName) of
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
    Result=case git_handler:read_file(CatalogRepoDir,FileName) of
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
    Result=case git_handler:read_file(CatalogRepoDir,FileName) of
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
		   ?LOG_WARNING("Failed git_handler:read_file ",[CatalogRepoDir,FileName,Error]),
		   {error,Error}
	   end,
    Result.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
which_filename(RepoDir,App)->
    {ok,AllFileNames}=git_handler:all_filenames(RepoDir),
    Result=find_filename(AllFileNames,RepoDir,App),
    Result.

find_filename(AllFileNames,RepoDir,App)->
    find_filename(AllFileNames,RepoDir,App,false).

find_filename(_AllFileNames,_RepoDir,_App,{ok,FileName})->
    {ok,FileName};
find_filename([],_RepoDir,_App,Found)->
    Found;
find_filename([FileName|T],RepoDir,App,false)->
    {ok,[Map]}=git_handler:read_file(RepoDir,FileName),
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
    case git_handler:is_repo_updated(RepoDir) of
	{error,["RepoDir doesnt exists, need to clone"]}->
	    GitClone=git_handler:clone(RepoDir,GitPath),
	    ?LOG_NOTICE("No Gitrepo- Cloned with result ",[GitClone]),
	    %io:format("RepoDir doesnt exists, need to clone ~p~n",[{GitClone,?MODULE,?FUNCTION_NAME,?LINE}]),
	    GitClone;
	false ->
	    GitUpdate=git_handler:update_repo(RepoDir),
	    ?LOG_NOTICE("Gitrepo not updated - Update repor with result ",[GitUpdate]),
	    GitUpdate;
	true ->
	    ok
    end,
    case filelib:is_dir(ApplicationDir) of
	false->
	    ok=file:make_dir(ApplicationDir);
	true->
	    ok
    end,
    {ok,AllFileNames}=git_handler:all_filenames(RepoDir),
    R=[{update_application(FileName,RepoDir,ApplicationDir),FileName}||FileName<-AllFileNames],
    []=[{X,FileName}||{X,FileName}<-R,
		      ok=/=X],

    ok.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
init(LocalRepoDir,GitPath,LocalApplicationDir)->
    {ok,CurrentDir}=file:get_cwd(),
    RepoDir=filename:join([CurrentDir,LocalRepoDir]),
    ApplicationDir=filename:join([CurrentDir,LocalApplicationDir]),
    file:del_dir_r(RepoDir),
    CloneResult=git_handler:clone(RepoDir,GitPath),
    ?LOG_NOTICE("Initial clone with result ",[CloneResult]),
    file:del_dir_r(ApplicationDir),
    MakeDirResult=file:make_dir(ApplicationDir),
    {ok,AllFileNames}=git_handler:all_filenames(RepoDir),
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
update_application(FileName,LocalCatalogRepoDir,LocalApplicationDir)->

    {ok,CurrentDir}=file:get_cwd(),
    CatalogRepoDir=filename:join([CurrentDir,LocalCatalogRepoDir]),
    ApplicationDir=filename:join([CurrentDir,LocalApplicationDir]),
    Result=case git_handler:read_file(CatalogRepoDir,FileName) of
	       {ok,[Info]}->
		   %io:format("Info,FileName ~p~n",[{Info,FileName,?MODULE,?LINE}]),
		   LocalRepoDir=maps:get(application_name,Info),
		   GitPath=maps:get(git,Info),
		   FullRepoDir=filename:join([ApplicationDir,LocalRepoDir]),
		   case git_handler:is_repo_updated(FullRepoDir) of
		       {error,["RepoDir doesnt exists, need to clone"]}->
			   GitClone=git_handler:clone(FullRepoDir,GitPath),
			   ?LOG_NOTICE("No Gitrepo for application - Cloned with result ",[LocalRepoDir,GitClone]),
			   %% GitClone: ok | {error,Reason}
			   GitClone;
		       false ->
			   GitUpdate=git_handler:update_repo(FullRepoDir),
			   %% GitClone: {ok,Result} | {error,Reason}
			   ?LOG_NOTICE("Application Gitrepo not updated - Update repor with result ",[GitUpdate]),
			   GitUpdate;
		       true ->
			   ok
		   end;
	       Error->
		   ?LOG_WARNING("Failed git_handler:read_file ",[CatalogRepoDir,FileName,Error]),
		   {error,Error}
	   end,
    Result.
