-module(throttle).
-behaviour(stream).

-export([start/2]).
-export([init/1, handle_message/2, terminate/1]).
-compile(export_all).

start(Interval, S) ->
	stream:start(?MODULE, [Interval, S]).

init([Interval, S]) ->
	Owner = self(),
	S:subscribe(
		fun(E) ->
			Owner ! {notify_data, E}
		end),
	{ok, [no_timer, Interval, none]}.

handle_message({notify_data, E}, [no_timer, Interval, _]=_State) ->
	{ok, Tref} = timer:send_after(Interval, timeout),
	{continue, [Tref, Interval, E]};
	
handle_message({notify_data, E}, [Tref, Interval, _]) ->
	timer:cancel(Tref),
	{ok, NTref} =  timer:send_after(Interval, timeout),
	{continue, [NTref, Interval, E]};

handle_message(timeout, [_, Interval, E]) ->
	{push_data, E, [no_timer, Interval, none]}.
terminate(_S) -> ok.
