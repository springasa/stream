-module(stream).
-export([start/2]).
-export([cast/2, subscribe/2, unsubscrible/1]).
-compile(export_all).

-opaque stream() :: {stream, pid()}.
-type closeable()::{stream, StreamPid::pid(),
			fun((term()) -> any())}.
-type observer()::fun((term()) -> any()). 
-export_type([stream/0]).

%% behavior callbacks
-callback init(Args :: term()) -> {ok, State::term()}.
-callback handle_message(Request::term(),
			    State::term()) ->
			    {push_data, Data::term() }|
			    {continue, NewState::term()} |
			    {stop, NewState::term()}.
-callback terminate(State::term()) -> Any :: term().

-spec start(module(), [term()]) -> stream().
start(Module, Args) ->
	Pid = spawn(fun() ->
			State = Module:init(Args),
			loop(Module, State, [])
		       end),
	{stream, Pid}.
	%ok.
-spec subscribe(observer(), stream()) -> closeable().
subscribe(Ob, {stream, Pid}) ->
	Pid ! {subscribe, Ob},
	{stream, Pid, Ob}.

-spec unsubscrible(closeable()) -> ok.
unsubscrible({stream, SPid, Ob}) ->
	SPid ! {unsubscrible, Ob},
	ok.

-spec cast(term(), stream()) -> ok.
cast(Message, {stream, Pid}) ->
	Pid ! Message,
	ok.
%%high order steam
-spec where(fun((term()) -> boolean()), stream()) -> stream().
where(Pred, Stream) ->
	where:start(Pred, Stream).
-spec select(fun((term()) -> term()), stream()) ->stream().
select(Trans, Stream) ->
	select:start(Trans, Stream).

-spec select_many(fun((term()) -> term()), stream()) ->stream().
select_many(Inflator,  Stream) ->
	select_many:start(Inflator, Stream).

-spec until(stream(), stream()) ->stream().
until(ES, S) ->
	until:start(ES, S).

-spec switch(stream()) ->stream().
switch(SOS) ->
	switch:start(SOS).

-spec throttle(integer(), stream()) -> stream().
throttle(Interval, S) ->
	throttle:start(Interval, S).

loop(Module, State, OBS) ->
	receive
		{subscribe, Ob} ->
			loop(Module, State, [Ob | lists:delete(Ob, OBS)]);
		{unsubscrible, Ob} ->
			loop(Module, State, lists:delete(Ob, OBS));
		Message ->
			case Module:handle_message(Message, State) of
				{push_data, Data, NewState} ->
					[Ob(Data) || Ob <- OBS],
					loop(Module, NewState, OBS);
				{continue, NewState} ->
					loop(Module, NewState, OBS);
				{stop, NewState} ->
					Module:terminate(NewState)
			end
	end.
