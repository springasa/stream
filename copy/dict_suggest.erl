-module(dict_suggest).
-export([start/0]).
-export([completions/2]).

-type dict_suggest() :: {dict_suggest, pid()}.
-spec start() -> dict_suggest().
start() ->
	Pid = spawn(fun() -> loop() end),
	{dict_suggest, Pid}.
-spec completions(string(), dict_suggest()) -> stream:stream().
completions(Word, {dict_suggest, Pid}) ->
	S = pass_through:start(),
	Pid ! {completions, S, Word},
	S.

loop() ->
	receive 
		{completions, S, Word} ->
			timer:send_after(200,
				{result, S, get_suggestions(Word)}),
			loop();
		{result, S, Suggestions} ->
			S:cast(Suggestions),
			%%S:stop(),
			loop()
	end.

get_suggestions(Word) ->
	case Word of 
		"t" -> ["tea"];
		Unknown -> [Unknown]
	end.