-module(switch).
-behaviour(stream).

-export([start/1]).
-export([init/1, handle_message/2, terminate/1]).
-compile(export_all).

-spec start(stream:stream()) ->
	stream:stream().
start(SOS) ->
	stream:start(?MODULE, [SOS]).

init([SOS]) ->
	Owner = self(),
	SOS:subscribe(
		fun(S) ->
			Owner ! {new_stream, S}
		end),
	{ok, []}.

handle_message({new_stream, S}, State) ->
	Owner = self(),
	Sub = S:subscribe(
		fun(E) -> 
			Owner ! {notify_data, E}
		end),
	case State of
		{ok, []} -> void;
		[PreSub] ->
			PreSub:unsubscribe()
	end,
	{continue, [Sub]};
handle_message({notify_data, E}, State) ->
	{push_data, E, State}.

terminate(_S) -> ok.
	
test_drag() ->
	MouseDown = pass_through:start(),
	MouseMove = pass_through:start(),
	MouseUp = pass_through:start(),
	((MouseDown:select(fun(_D) -> MouseMove:until(MouseUp) end))
	:switch())
	:subscribe(fun(Pos) -> io:format("mouse drag ~p~n", [Pos]) end),
	{MouseDown, MouseMove, MouseUp}.