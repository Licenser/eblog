%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2011, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2011 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(index).
-compile(export_all).
-include_lib("include/blog.hrl"). 

string_join(Items, Sep) ->
    lists:flatten(lists:reverse(string_join(Items, Sep, []))).
string_join([Head | []], _Sep, Acc) ->
    [Head | Acc];
string_join([Head | Tail], Sep, Acc) ->
    string_join(Tail, Sep, [Sep, Head | Acc]).

render() ->
    layout:render([{title, config_srv:get(name)},
                   {body, mustache:render(index,"templates/index.mustache",dict:new())}]).

posts() ->
    {atomic, Posts} = db:select_all(),
    string_join([post:render(Post) || Post <- Posts],"<br/>").
