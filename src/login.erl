%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2011, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 28 Jul 2011 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(login).
-compile(export_all).
-include_lib("include/blog.hrl"). 


render() ->
    layout:render([{title, "Admin - licenser.net"},
                   {body, mustache:render(login,"templates/login.mustache",dict:new())}]).
