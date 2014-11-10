-module(until).
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
	Sub1 = S:subscribe(
		fun(E) -> 
			Owner ! {notify_data, E}
		end),
	Sub2 = ES:subscribe(
		fun(_)->
			Owner ! end_data
		end),
	{ok, [Sub1, Sub2]}.

handle_message({notify_data, E}, State) ->
	{push_data, E, State};
handle_message({end_data, [Sub1, Sub2]}, State) ->
	Sub1:unsubscribe(),
	Sub2:unsubscribe(),
	{stop, State}.	
terminate(_S) -> ok.

test_until() ->
	MouseDown = pass_through:start(),
	MouseMove = pass_through:start(),
	MouseUp = pass_through:start(),
	(MouseDown:select_many(
		fun(_D) ->
			MouseMove:until(MouseUp)
		end))
	:subscribe(fun(Pos) -> 
			io:format("mouse drag: ~p~n", [Pos]) 
		    end),
	{MouseDown, MouseMove, MouseUp}.