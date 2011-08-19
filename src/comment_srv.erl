%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <licenser@171.10.20.172.in-addr.arpa.noptr.antlabs.com>
%%% @copyright (C) 2011, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 14 Aug 2011 by Heinz N. Gies <licenser@171.10.20.172.in-addr.arpa.noptr.antlabs.com>
%%%-------------------------------------------------------------------
-module(comment_srv).
-behaviour(gen_server).

-include("blog.hrl").

%% API
-export([start_link/0, verify/0, check/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {server = "Eblog/1.0.0", webside = "http://licenser.net", key = "330b47d048df"}).

%%%===================================================================
%%% API
%%%===================================================================

check(Comment) ->
    gen_server:call(?SERVER, {check, Comment}).

verify() ->
    gen_server:cast(?SERVER, verify).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    inets:start(),
    {ok, #state{server = config_srv:get(server),
		webside = config_srv:get(url),
		key = config_srv:get(akismet_api_key)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({check, #comment{ip=UserIP, 
			     user_agent=UserAgent,
			     referrer=Referrer,
			     post_id=PostID,
			     type=Type,
			     author=Author,
			     email=Email,
			     id = ID,
			     body = CommentContent
			    } = Comment}, 
	    _From, 
	    #state{server = Server, webside = Webside, key = Key} = State) ->
    Permalink = Webside ++ "/posts/" ++ PostID,
    CommentURL = Webside ++ "/posts/" ++ PostID ++ "/comments/" ++ ID,
    Body = "blog=" ++ edoc_lib:escape_uri(Webside) ++
	"&user_ip=" ++ edoc_lib:escape_uri(UserIP) ++
	"&user_agent=" ++ edoc_lib:escape_uri(UserAgent) ++
	"&referrer=" ++ edoc_lib:escape_uri(Referrer) ++
	"&permalink=" ++ edoc_lib:escape_uri(Permalink) ++
	"&comment_type=" ++ edoc_lib:escape_uri(Type) ++
	"&comment_author=" ++ edoc_lib:escape_uri(Author) ++
	"&comment_author_email=" ++ edoc_lib:escape_uri(Email) ++
	"&comment_author_url=" ++ edoc_lib:escape_uri(CommentURL) ++
	"&comment_content=" ++ edoc_lib:escape_uri(CommentContent),
    UA = Server ++ " | ErlangAkismet/1.0.0",
    Url = "http://" ++ Key ++ ".rest.akismet.com/1.1/comment-check",
    case httpc:request(post,
		       {Url, [{"User-Agent", UA}],
			"application/x-www-form-urlencoded", Body},
		       [], [{body_format, string}]) of
	{ok,{_Reply, _Header, "true"}} -> handle_spam(Comment#comment{rating=spam}),
					  Reply = spam;
	{ok,{_Reply, _Header, "false"}} -> handle_ham(Comment#comment{rating=ham}),
					   Reply = ham
    end,
        {reply, Reply, State};
handle_call(verify, _From, #state{server = Server, webside = Webside, key = Key} = State) ->
    UA = Server ++ " | ErlangAkismet/1.0.0",
    Url = "http://rest.akismet.com/1.1/verify-key",
    Body = "key=" ++ Key ++"&blog=" ++ Webside,
    io:format("URL >~p~nUA  >~p~nBODY>~p~n", [Url, UA, Body]),
    Result =
	httpc:request(post,
		      {Url, [{"User-Agent", UA}],
		       "application/x-www-form-urlencoded", Body},
		      [], [{body_format, string}]),
    io:format("RES >~p~n", [Result]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_spam(Comment) ->
    db:insert_comment(Comment).
handle_ham(Comment) ->
    db:insert_comment(Comment).
