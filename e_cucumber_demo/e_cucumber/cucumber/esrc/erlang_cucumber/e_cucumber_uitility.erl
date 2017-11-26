-module(e_cucumber_uitility).
-include("e_cucumber_utility.hrl").
-compile(export_all).

load_json_obj(Dir) ->
	{ok, Bin} = file:read_file(Dir),
	{ok, Obj, _Remain} = rfc4627:decode(Bin),
	% io:format("Remain ~n~p~n",[Remain]),
	Obj.

load_json_all_objs(Dir) ->
	{ok, Bin} = file:read_file(Dir),
	do_load_json_all_objs(Bin,[]).
do_load_json_all_objs([], Acc) -> Acc;	
do_load_json_all_objs(Bin, Acc) ->
	{ok, {obj, [{"type", Type}|_T]}=Obj, Remain} = rfc4627:decode(Bin),
	NewAcc=case lists:keyfind(Type, 1, Acc) of
		false -> Acc ++ [{Type,[Obj]}];
		{Type, OldList} -> 
		lists:keyreplace(Type,1,Acc, {Type,OldList++[Obj]})
	end,
	do_load_json_all_objs(Remain, NewAcc).

get_loc(Loc) ->
	{obj,[{"line", Line}, {"column", Col}]} = Loc,
	{Line, Col}.

print_n_space(N) when N > 0 ->
	[io:format(" ") || _I <- lists:seq(1, N)];
print_n_space(_N) -> ok.

print_line_no(StrLen, Line) ->
	print_n_space(?MAX_CH - StrLen),
	io:format("~s #line ~p ~s~n", [?CONSOLE_COLOR_GRAY, Line, ?CONSOLE_COLOR_NORMAL]).