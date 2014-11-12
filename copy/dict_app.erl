-module(dict_app).
-export([start/0]).

start() ->
	GUI = gui:start(),
	ECS = entry_change_stream(GUI),
	DSServer = dict_suggest:start(),

	(((((ECS:where(fun(Word) -> string:strip(Word) =/= "" end))
		:distinct_until_changed())
		:throttle(200))
		:select(fun(D) -> DSServer:completions(D) end))
	              :switch())
	              :subscribe(fun(Words) ->
	              	GUI:set_words(Words) end).
entry_change_stream(GUI) ->
	S = pass_through:start(),
	GUI:add_text_change_listener(S),
	S.