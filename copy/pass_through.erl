-module(pass_through).
-behaviour(stream).
-export([start/0]).
-export([init/1, handle_message/2, terminate/1]).

start() ->
	stream:start(?MODULE, []).
init([]) ->
	{ok, []}.
handle_message(Msg, State) ->
	{push_data, Msg, State}.

terminate(_S) ->
	 ok.