-module(select_many).
-behaviour(stream).

-export([start/2]).
-export([init/1, handle_message/2, terminate/1]).
-compile(export_all).

-spec start(fun((term()) -> boolean()), stream:stream()) ->
	stream:stream().
start(PF, Stream) ->
	stream:start(?MODULE, [PF, Stream]).
init([PF, Stream]) ->
	Owner = self(),
	Stream:subscribe(
		fun(A) ->
			(PF(A)) : subscribe(
				fun(B) ->
					Owner ! {notify_data, B}
				end)
		end),	
	{ok, []}.

handle_message({notify_data, E}, State) ->
	{push_data, E, State}.
terminate(_S) -> ok.

mouse_down_stream() ->
	pass_through:start().
mouse_move_stream() ->
	pass_through:start().
test_select_many() ->
	MouseDown = mouse_down_stream(),
	MouseMove = mouse_move_stream(),
	(MouseDown:select_many(fun(_D) -> MouseMove end))
		         :subscribe(fun(Pos) ->
		         			io:format("mouse drag ~p~n", [Pos])
		         		end),
	{MouseDown, MouseMove}.	