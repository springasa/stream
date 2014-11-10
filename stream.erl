-module(stream).
-compile(export_all).

init() ->
	loop([]).
loop(OBS) ->
	receive
		{subscribe,  Ob}  ->
			loop([Ob | OBS]);
		{unsubscribe, Ob} ->
			loop(lists:delete(Ob, OBS));
		stop -> 
			stop;
		Msg ->
			[Ob(Msg) || Ob <- OBS],
			loop(OBS)
	end.

start() ->
	Pid = spawn(fun() -> init() end),
	{stream, Pid}.

cast(Msg, {stream, Pid}) ->
	Pid ! Msg,
	ok.
stop({stream, Pid}) ->
	Pid ! stop,
	ok.
subscribe(Ob, {stream, Pid}) ->
	Pid ! {subscribe,  Ob},
	ok.

unsubscribe(Ob, {stream, Pid}) ->
	Pid ! {unsubscribe,  Ob},
	ok.

where(Pred,  Stream) ->
	WS = stream:start(),
	Stream:subscribe(
		fun (D) ->
			case Pred(D) of 
				true -> 
					WS:cast(D);
				_->
					pass
			end
		end),
	WS.

select(Func,  Stream) ->
	WS = stream:start(),
	Stream:subscribe(
		fun(D) ->
			 WS:cast(Func(D))
		end),
	WS.

select_many(Inflator,  Stream) ->
	SM = stream:start(),
	Stream:subscribe(
		fun(A) ->
			(Inflator(A)) : subscribe(
				fun(B) ->
					SM:cast(B)
				end)
		end),
	SM.	
%-spec until(stream(), stream()) -> stream().

%S = stream:start(), 
%S:subscribe(fun(Msg) -> io:format("Msg ~p~n", [Msg]) end),
%S:cast("hello")

% S=stream:start(),
% (S:where(fun(D) -> is_integer(D) end))
%   :subscribe(fun(Msg) -> io:format("Msg ~p~n", [Msg]) end),
% S:cast("Hello"),
% S:cast(1).

until(ES, S) ->
	US = stream:start(),
	Ob1 = fun(E) -> US:cast(E) end,
	Ob2 = fun(_) -> 
		S:unsubscribe(Ob1),
		US:stop() 
	           end,
	S:subscribe(Ob1),
	ES:subscribe(Ob2),	
	US.
	%ES:unsubscribe(Ob2),

%until(MouseMove, MouseUp):subscribe(print);

test2() ->
	S = stream:start(),
	((S:where(fun(D) -> is_integer(D) end))
	  :select(fun(D) -> D*2 end))
	  :subscribe(fun(Msg) -> io:format("Msg ~p~n", [Msg]) end),
	S:cast("hello"),
	S:cast(1).

mouse_down_stream() ->
	stream:start().
mouse_move_stream() ->
	stream:start().
test_3() ->
	MouseDown = mouse_down_stream(),
	MouseMove = mouse_move_stream(),
	(MouseDown:select_many(fun(_D) -> MouseMove end))
		         :subscribe(fun(Pos) ->
		         			io:format("mouse drag ~p~n", [Pos])
		         		end),
	{MouseDown, MouseMove}.
%% type in shell
%	{MouseDown, MouseMove} = stream:test_3(),	      
%	MouseDown:cast(down),
%	MouseMove:cast({10,10}).

test_4() ->
	MouseDown = mouse_down_stream(),
	MouseMove = mouse_move_stream(),
	MouseUp = mouse_down_stream(),
	until(MouseMove, MouseUp).
