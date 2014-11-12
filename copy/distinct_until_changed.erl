-module(distinct_until_changed).
-behaviour(stream).

-export([start/1]).
-export([init/1, handle_message/2, terminate/1]).

start(S) ->
	stream:start(?MODULE, [S]).
init([S]) ->
	Owner = self(),
	S:subscribe(
		fun(E) ->
			Owner ! {notify_data, E}
		end),
	{ok, []}.
handle_message({notify_data, Data}, State) ->
	case State of
		{ok, []} -> {continue, [Data]};
		[OldData] ->
			 case OldData=:=Data of 
			 	true -> {continue, [Data]};
			 	false -> {push_data, Data, State}
			 end	
	end.

terminate(_S) -> ok.