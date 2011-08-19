% ==========================================================================================================
% MISULTIN - Example: Application based on Misultin - MAIN APPLICATION GEN_SERVER
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>
% All rights reserved.
%
% BSD License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%	 following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%	 the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%	 products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
% ==========================================================================================================
-module(web_srv).
-behaviour(gen_server).

-include("blog.hrl").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/0, handle_http/3]).


% ============================ \/ API ======================================================================

% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the server.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

% ============================ /\ API ======================================================================


% ============================ \/ GEN_SERVER CALLBACKS =====================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
% Description: Initiates the server.
% ----------------------------------------------------------------------------------------------------------
init([]) ->
    {ok, {}}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_call(Request, From, State) -> {reply, Reply, State} | {reply, Reply, State, Timeout} |
%									   {noreply, State} | {noreply, State, Timeout} |
%									   {stop, Reason, Reply, State} | {stop, Reason, State}
% Description: Handling call messages.
% ----------------------------------------------------------------------------------------------------------

% handle_call generic fallback
handle_call(_Request, _From, State) ->
    {reply, undefined, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_cast(Msg, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling cast messages.
% ----------------------------------------------------------------------------------------------------------

% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
    {noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_info(Info, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling all non call/cast messages.
% ----------------------------------------------------------------------------------------------------------

% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
    {noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: terminate(Reason, State) -> void()
% Description: This function is called by a gen_server when it is about to terminate. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
% ----------------------------------------------------------------------------------------------------------
terminate(_Reason, _State) ->
    terminated.

% ----------------------------------------------------------------------------------------------------------
% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
% Description: Convert process state when code is changed.
% ----------------------------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% ============================ /\ GEN_SERVER CALLBACKS =====================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% ---------------------------- \/ misultin requests --------------------------------------------------------

html(Req, T) ->
    Req:ok([{"Content-Type", "text/html"}], T).

handle_http('GET', ["admin"], Req) ->
    Admin = config_srv:get(admin_user),
    case Req:get_cookie_value("user", Req:get_cookies()) of
        Admin -> html(Req, admin:render());
        _ -> html(Req, login:render())
    end;
handle_http('POST', ["admin"], Req) ->
    Admin = config_srv:get(admin_user),
    Pass = config_srv:get(admin_pass),
    case Req:parse_post() of
        [{"User",Admin},{"Password",Pass}] ->
            Req:ok([Req:set_cookie("user", "heinz", [{max_age, 365*24*3600}]), {"Content-Type", "text/html"}], admin:render());
        _ ->
           html(Req, login:render())
    end;
handle_http('POST', ["post", PostID, "comment"], Req) ->
    Headers = dict:from_list(Req:get(headers)),
    {IpA, IpB, IpC, IpD} = Req:get(peer_addr),
    IP = erlang:integer_to_list(IpA) ++ "." ++
	erlang:integer_to_list(IpB) ++ "." ++
	erlang:integer_to_list(IpC) ++ "." ++
	erlang:integer_to_list(IpD),
    case db:select(PostID) of
	not_found -> html(Req, index:render());
	_ ->  [{"nick", Nick},
	       {"comment", Comment}] = Req:parse_post(),
	      comment_srv:check(#comment{
				   post_id=PostID,
				   ip=IP,
				   date=erlang:localtime(),
				   user_agent=case dict:find('User-Agent', Headers) of
						  {ok, Res} -> Res;
						  _ -> ""
					      end,
				   referrer=case dict:find('Referrer', Headers) of
						{ok, Res} -> Res;
						_ -> ""
					    end,
				   type="comment",
				   author=Nick,
				   body=Comment}),
	      html(Req, index:render())
    end;
handle_http('POST', ["post"], Req) ->
    case Req:get_cookie_value("user", Req:get_cookies()) of
        "heinz" -> 
            [{"title", Title},
             {"post", Post},
             {"keywords", Keywords}] = Req:parse_post(),
            db:insert(Title, re:split(Keywords, ",\s*", [{return,list}]), Post),
            html(Req, admin:render());
        _ ->html(Req, login:render())         
    end;
handle_http('GET', ["feed"], Req) -> 
    Req:ok([{"Content-Type", "application/rss+xml"}], rss:render());
handle_http('GET', ["feed", "atom"], Req) -> 
    Req:ok([{"Content-Type", "application/atom+xml"}], atom:render());
handle_http('GET', ["style", "blog.css"], Req) ->
    Req:file("priv/style/blog.css");
handle_http('GET', ["favicon.ico"], Req) ->
    Req:respond(404, [], "");
handle_http('GET', _, Req) -> 
    html(Req, index:render()).

% ---------------------------- /\ misultin requests --------------------------------------------------------

% ============================ /\ INTERNAL FUNCTIONS =======================================================

