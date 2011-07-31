%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2011, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2011 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(post).
-compile(export_all).
-include_lib("include/blog.hrl"). 


%% id
%% title
%% body
%% keywords


to_dict(Post) ->
    dict:from_list([
                    {id, Post#post.id},
                    {title, Post#post.title},
                    {body, Post#post.body},
                    {keywords, [dict:from_list([{keyword, Keyword}]) || Keyword <- Post#post.keywords]}
                   ]).
render(Post) ->
    mustache:render(post,mustache:compile(post),to_dict(Post)).
