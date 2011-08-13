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

date_to_str({{Y,Mo,D},{H,Mi,_}}) ->
    io_lib:format("~p-~p-~p ~p:~p", [Y, Mo, D, H, Mi]).

comment_to_dict(Comment) ->
    dict:from_list([
                    {comment_nick, Comment#comment.nick},
                    {comment_date, date_to_str(Comment#comment.date)},
                    {comment_body, markdown:conv(Comment#comment.body)}
                   ]).
    

to_dict(Post) ->
    dict:from_list([
                    {id, Post#post.id},
                    {title, Post#post.title},
                    {date, Post#post.date},
                    {body, markdown:conv(Post#post.body)},
		    {comments, [comment_to_dict(Comment) || Comment <- db:comments(Post#post.id)]},
                    {keywords, [dict:from_list([{keyword, Keyword}]) || Keyword <- Post#post.keywords]}
                   ]).
render(Post) ->
    mustache:render(post,"templates/post.mustache",to_dict(Post)).
