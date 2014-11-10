-module(select).
-behaviour(stream).

-export([start/2]).
-export([init/1, handle_message/2, terminate/1]).
-compile(export_all).

-spec start(fun((term()) -> boolean()), stream:stream()) ->
	stream:stream().
start(ES, S) ->
	stream:start(?MODULE, [ES, S]).
init([ES, S]) ->
	Owner = self(),
	Stream:subscribe(
		fun(D) ->
			Owner ! {notify_data,PF(D)}
		end),
	{ok, []}.

handle_message({notify_data, E}, State) ->
	{push_data, E, State}.
terminate(_S) -> ok.

test_select() ->
	S = pass_through:start(),
	((S:where(fun(D) ->  is_integer(D) end))	
	   :select(fun(D) -> D*2 end))
	   : subscribe(fun(D) -> 
	  		io:format("data: ~p~n", [D])
	  	       end),
	S.