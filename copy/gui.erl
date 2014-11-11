-module(gui).
-export([start/0]).
-export([set_words/2, add_text_change_listener/2]).

-type gui()::{gui, pid()}.

-spec start() -> gui().

start() ->
	Pid = spawn(fun()->init() end),
	{gui, Pid}.
-spec set_words([string()], gui()) -> ok.
set_words(Words, {gui, Pid}) ->
	Pid ! {set_words, Words},
	ok.
-spec add_text_change_listener(stream:stream(), gui()) -> ok.
add_text_change_listener(Stream, {gui, Pid}) ->
	Pid ! {add_text_change_listener, Stream},
	ok.
init() ->
	GS = gs:start(),
	Win = gs:window(GS, [{width, 490}, {height, 490},
		{title, "Dictionary Completion"}]),
	Entry = gs:entry(Win, [{x, 5}, {y, 35}, {width, 480},
		{keypress, true}, {setfocus, true}]),
	ListBox = gs:listbox(Win, [{x, 5}, {y, 65}, {width, 480}, {height, 420},
		{vscroll, right}]),
	gs:config(ListBox, [{items, []}]),
	gs:config(Win, [{map, true}]),
	loop(Entry, ListBox, []).

loop(Entry, ListBox, Listeners) ->
	receive 
		{set_words, Words} ->
			gs:config(ListBox, [{items, Words}]),
			loop(Entry, ListBox, Listeners);
		{add_text_change_listener, Stream} ->
			loop(Entry, ListBox, [Stream|Listeners]);
		{gs, Entry, keypress, _, _} ->
			W = gs:read(Entry, text),
			[Stream:cast(W) || Stream <- Listeners],
			loop(Entry, ListBox, Listeners);
		{gs, _, destroy, _, _} ->
			stop
	end.