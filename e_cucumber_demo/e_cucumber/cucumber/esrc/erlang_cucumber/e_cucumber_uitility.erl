-module(e_cucumber_uitility).
-include("e_cucumber_utility.hrl").
-compile(export_all).

load_json_obj(Dir) ->
	{ok, Bin} = file:read_file(Dir),
	{ok, Obj, []} = rfc4627:decode(Bin),
	Obj.

get_loc(Loc) ->
	{obj,[{"line", Line}, {"column", Col}]} = Loc,
	{Line, Col}.

print_n_space(N) when N > 0 ->
	[io:format(" ") || _I <- lists:seq(1, N)];
print_n_space(_N) -> ok.

print_line_no(StrLen, Line) ->
	print_n_space(?MAX_CH - StrLen),
	io:format("~s #line ~p ~s~n", [?CONSOLE_COLOR_GRAY, Line, ?CONSOLE_COLOR_NORMAL]).