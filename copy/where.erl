-module(where).
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
		fun(E) ->
			case PF(E)  of
				true ->
					Owner ! {notify_data, E};
				_-> pass
			end
		end
		),
	{ok, []}.

handle_message({notify_data, E}, State) ->
	{push_data, E, State}.
terminate(_S) -> ok.

test_where() ->
	S = pass_through:start(),
	(S:where(fun(D) ->
			 is_integer(D) 
		end))
	  : subscribe(fun(D) -> 
	  		io:format("data: ~p~n", [D])
	  	       end),
	S.