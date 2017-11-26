-module(scan_gherkin_doc_json).
-include("e_cucumber_utility.hrl").
-compile(export_all).
-import(e_cucumber_uitility, [load_json_obj/1, get_loc/1, print_line_no/2, print_n_space/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       1. run      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run(GherkinDocJsonDir) ->
	DocObj = load_json_obj(GherkinDocJsonDir),
	{AllSteps,AllParseRes} = parse_doc_json(DocObj),
	% io:format("~p~n", [AllParseRes]),
	{AllSteps,AllParseRes}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 2. parse_doc_json %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_doc_json({obj, DocObjList}) -> 
	% io:format("~p~n", [DocObjList]),
	[{"type", _}, {"uri", _}, {"document", DocumentVal}] = DocObjList,
	{AllSteps,AllParseRes} = parse_document_json(DocumentVal),
	{AllSteps,AllParseRes}.
%%%%%%%%%%%%%%% 2.1 document %%%%%%%%%%%%%%%
parse_document_json({obj, DocumentObjList}) -> 
	% io:format("~p~n", [DocumentObjList]),
	[{"type", _}, {"feature", FeatureVal}, {"comments", _}] = DocumentObjList,
	{AllSteps,AllParseRes} = parse_feature_json(FeatureVal),
	{AllSteps,AllParseRes}.

%%%%%%%%%%%%%%% 2.1.1 feature %%%%%%%%%%%%%%%
parse_feature_json({obj, [{"type", _}, {"tags", _},  {"location", _Loc},
	                       {"language", _},  {"keyword", KeyWord}, 
	                       {"name", Name}, {"description", Des}, 
	                       {"children", Children}] = _FeatureObjList}) -> 
	 io:format("~n~s: ~s~n", [KeyWord, Name]),
	 io:format("~s~n", [Des]),
	 {AllSteps,AllParseRes}=parse_feature_children_json(Children, [],[]),
	{AllSteps,AllParseRes};
parse_feature_json({obj, [{"type", _}, {"tags", _},  {"location", _Loc},
	                       {"language", _},  {"keyword", KeyWord}, 
	                       {"name", Name},   {"children", Children}] 
	                       = _FeatureObjList}) -> 
	 io:format("~n~s: ~s~n", [KeyWord, Name]),
	 {AllSteps,AllParseRes}=parse_feature_children_json(Children, [], []),
	{AllSteps,AllParseRes}.

%%%%%%%%%%%%%%% 2.1.1.1 feature_children %%%%%%%%%%%%%%%
parse_feature_children_json([], AllSteps, AllParseRes) -> {AllSteps, AllParseRes};
parse_feature_children_json([H|T], AllSteps, AllParseRes) ->
	{Steps, ParseFeatureRes} = do_parse_feature_child_json(H),
	NewSteps = AllSteps ++ Steps,
	NewParseRes = AllParseRes ++ ParseFeatureRes,
	parse_feature_children_json(T, NewSteps,NewParseRes).

% do_parse_scenario_outline
do_parse_feature_child_json({obj, [{"type", <<"ScenarioOutline">>}, {"tags", _},
	                                {"location", Loc}, {"keyword", KeyWord}, 
	                                {"name", Name}, {"steps", Steps}, 
	                                {"examples", Exmp}]=_ChildObjList})->
	% io:format("~p~n", [ChildObjList]),
	ParseFeatureRes=parse_feature_json(Loc, KeyWord, Name, Steps),
	parse_examples_json(Exmp),
	{Steps, ParseFeatureRes};
do_parse_feature_child_json({obj, [{"type",  <<"Background">>},
	                                {"location", Loc}, {"keyword", KeyWord}, 
	                                {"name", Name}, {"steps", Steps}]=_ChildObjList})->
	ParseFeatureRes=parse_feature_json(Loc, KeyWord, Name, Steps),
	{Steps, ParseFeatureRes};
do_parse_feature_child_json({obj, [{"type",  <<"Scenario">>}, {"tags", _},
	                                {"location", Loc}, {"keyword", KeyWord}, 
	                                {"name", Name}, {"steps", Steps}]=_ChildObjList})->
	ParseFeatureRes=parse_feature_json(Loc, KeyWord, Name, Steps),
	{Steps, ParseFeatureRes}.
parse_feature_json(Loc, KeyWord, Name, Steps) ->
	{Line, _Col} = get_loc(Loc),
	io:format("~n  ~s: ~s", [KeyWord, Name]),
	StrLen = 3 + erlang:length(binary_to_list(KeyWord)) + erlang:length(binary_to_list(Name)),
	print_line_no(StrLen,Line),
	parse_steps_json(Steps,[]).

%%%%%%%%%%%%%%% 2.1.1.1.1 steps %%%%%%%%%%%%%%%
parse_steps_json([], StepsParseRes) -> StepsParseRes;
parse_steps_json([H | T], StepsParseRes) -> 
	CurStepRes=do_parse_one_step_json(H),
	parse_steps_json(T, StepsParseRes++[CurStepRes]).
do_parse_one_step_json({obj, [{"type", _},      
	                          {"location", Loc},
	                          {"keyword", KeyWord}, 
	                          {"text", Text}]}) ->
	{Line, _Col} = get_loc(Loc),
	io:format("~s    ~s ~s ~s", [?CONSOLE_COLOR_BLUE, KeyWord, Text, ?CONSOLE_COLOR_NORMAL]),
	StrLen = 5 + erlang:length(binary_to_list(KeyWord)) + erlang:length(binary_to_list(Text)),
	print_line_no(StrLen,Line),
	step_no_data_list_arg;
do_parse_one_step_json({obj, [{"type", _},      
	                          {"location", Loc},
	                          {"keyword", KeyWord}, 
	                          {"text", Text},
	                          {"argument", Arg}]}) ->
	{Line, _Col} = get_loc(Loc),
	io:format("~s    ~s ~s ~s", [?CONSOLE_COLOR_BLUE, KeyWord, Text, ?CONSOLE_COLOR_NORMAL]),
	StrLen = 5 + erlang:length(binary_to_list(KeyWord)) + erlang:length(binary_to_list(Text)),
	print_line_no(StrLen,Line),
	{obj, [{"type", _}, {"location", ArgLoc}, {"rows", Rows}]} = Arg,
	parse_step_arg_info(ArgLoc, Rows).

parse_step_arg_info(_ArgLoc, ArgRows) -> 
	AllArgVals=parse_tb_body_json(ArgRows,[]),
	% io:format("---------------------~p~n", [AllArgVals]),
	AllArgVals.
	
%%%%%%%%%%%%%%% 2.1.1.1.2 examples %%%%%%%%%%%%%%%
parse_examples_json([]) -> ok;
parse_examples_json([H|T]) -> 
	do_parse_one_example_json(H),
	parse_examples_json(T).
do_parse_one_example_json({obj, ExamObjList}) ->
	%io:format("~p~n", [ExamObjList]),
	[{"type", _},      {"location", _Loc},
	 {"keyword", KeyWord}, {"name", _}, {"tags", _},
	 {"tableHeader", TbHead}, {"tableBody", TbBody}] = ExamObjList,
	io:format("~n    ~s~n", [KeyWord]),
	HeadCellVal=parse_tb_head_json(TbHead),
	BodyCellVal=parse_tb_body_json(TbBody,[]),
	HeadCellVal ++ BodyCellVal.

%%%%%%%%%%%%%%% 2.1.1.1.2.1 tableHeader %%%%%%%%%%%%%%%
parse_tb_head_json({obj, TbHeadObjList}) ->
	[{"type", _}, {"location", _Loc}, {"cells", Cells}] = TbHeadObjList,
	HeadCellVal=parse_tb_cells_json(tb_head, {1, 0}, Cells,[]),
	[HeadCellVal].
parse_tb_cells_json(_Type, {_LastCol, LastLens}, [],CellVals) -> 
	print_n_space(10-LastLens),
	io:format("|~n"),
	list_to_tuple(CellVals);
parse_tb_cells_json(Type, {LastCol, _LastLens}, [H|T], CellVals) ->
	{CurCol, CurLens, CellValue}=do_parse_one_cell_json(Type, LastCol, H),
	parse_tb_cells_json(Type, {CurCol, CurLens}, T, CellVals ++ [CellValue]).
do_parse_one_cell_json(Type, LastCol, {obj, CellObjList}) ->
	[{"type", _}, {"location", Loc}, {"value", CellValue}] = CellObjList,
	{_Line, Col} = get_loc(Loc),
	print_n_space(Col - LastCol),
	print_cell_val(Type, CellValue),
	Lens = length(binary_to_list(CellValue)),
	{Col + Lens, Lens, CellValue}.
print_cell_val(tb_head, CellVal) ->
	io:format("| ~s~s~s", [?CONSOLE_COLOR_YELLOW, CellVal, ?CONSOLE_COLOR_NORMAL]);
print_cell_val(tb_body, CellVal) ->
	io:format("| ~s~s~s", [?CONSOLE_COLOR_PURPLE, CellVal, ?CONSOLE_COLOR_NORMAL]).

%%%%%%%%%%%%%%% 2.1.1.1.2.1 tableBody %%%%%%%%%%%%%%%
parse_tb_body_json([],AllRowVal) -> AllRowVal;
parse_tb_body_json([H|T],AllRowVal) ->
	OneRowVal=do_parse_tb_body_one_row_json(H),
	parse_tb_body_json(T, AllRowVal ++ [OneRowVal]).
do_parse_tb_body_one_row_json({obj, TbBodyRowObjList}) ->
	[{"type", _}, {"location", _Loc}, {"cells", Cells}] = TbBodyRowObjList,
	parse_tb_cells_json(tb_body, {1, 0}, Cells,[]).
