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


render(Post) ->
    mustache:render(post,mustache:compile(post),
                    dict:from_list([
                                    {id, Post#post.id},
                                    {title, Post#post.title},
                                    {body, Post#post.body},
                                    {keywords, Post#post.keywords}
                                   ])).
