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

-export([start/0, insert/3, select_all/0]).

start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(post,
                        [{disc_copies, [node()] },
                         {attributes,      
                          record_info(fields,post)} ]).



id() ->
    {A, B, C} = erlang:now(),
    <<A:16,B:32,C:32>>.

insert(Title, Keywords, Body) ->
    uuid:init(),
    Fun = fun() ->
                  mnesia:write(
                    #post{
                       id=uuid:to_string(uuid:v4()),
                       title=Title,
                       keywords=Keywords, 
                       date=erlang:localtime(),
                       body=Body
                      })
          end,
    mnesia:transaction(Fun).

select(Index) ->
    Fun = 
        fun() ->
                mnesia:read({post, Index})
        end,
    {atomic, [Row]}=mnesia:transaction(Fun),
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
