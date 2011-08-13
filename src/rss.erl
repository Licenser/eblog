%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2011, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2011 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(rss).
-compile(export_all).
-include_lib("include/blog.hrl"). 

render() ->
    mustache:render(rss,"templates/rss.mustache",dict:new()).

posts() ->
    {atomic, Posts} = db:select_all(),
    [post:to_dict(Post) || Post <- Posts].
