%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2011, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2011 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(layout).
-compile(export_all).

render(TVars) when is_list(TVars) ->
    mustache:render(layout,mustache:compile(layout),dict:from_list(TVars)).
