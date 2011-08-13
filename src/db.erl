%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2011, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2011 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(db).
-include("blog.hrl").
-include_lib("stdlib/include/qlc.hrl"). 

-export([start/0, insert/3, select_all/0, comments/1, insert_comment/3, select/1]).

start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(post,
                        [{disc_copies, [node()] },
                         {attributes,      
                          record_info(fields,post)} ]),
    mnesia:create_table(comment,
                        [{disc_copies, [node()] },
                         {attributes,      
                          record_info(fields,comment)}]).


insert(Title, Keywords, Body) ->
    uuid:init(),
    Fun = fun() ->
                  mnesia:write(#post{
				  id=uuid:to_string(uuid:v4()),
				  title=Title,
				  keywords=Keywords, 
				  date=erlang:localtime(),
				  body=Body})
          end,
    mnesia:transaction(Fun).

insert_comment(PostID, Nick, Body) ->
    uuid:init(),
    Fun = fun() ->
                  mnesia:write(#comment{
				  id=uuid:to_string(uuid:v4()),
				  post=PostID,
				  nick=Nick,
				  date=erlang:localtime(),
				  body=Body})
          end,
    mnesia:transaction(Fun).


select(Index) ->
    Fun = fun() ->
		  mnesia:read({post, Index})
	  end,
    case mnesia:transaction(Fun) of
	{atomic, [Row]} -> Row;
	{atomic, []} -> not_found;
	Else -> Else
    end.


comments(PostID) ->
    {atomic, Row} = mnesia:transaction( 
		      fun() ->
			      qlc:eval(qlc:q(
					 [ C || #comment{post = ThisPost} = C <- mnesia:table(comment),
						string:str(ThisPost, PostID) > 0])) 
		      end),
    Row.

select_all() -> 
    mnesia:transaction( 
      fun() ->
              qlc:eval(qlc:q(
                         [ X || X <- mnesia:table(post) ] 
                        )) 
      end).

select_search( Word ) -> 
    mnesia:transaction( 
      fun() ->
              qlc:eval(qlc:q(
                         [ {F0,F1,F2,F3} || 
                             {F0,F1,F2,F3} <- 
                                 mnesia:table(post),
                             (string:str(F2, Word)>0) or  
                                                        (string:str(F3, Word)>0)
                         ])) 
      end ).
