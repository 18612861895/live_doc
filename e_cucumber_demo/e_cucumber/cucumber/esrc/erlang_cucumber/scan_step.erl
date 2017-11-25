-module(scan_step).
-include("e_cucumber_utility.hrl").
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run(AllSteps, StepDefDir) -> 
	ExsitSteps = get_all_exsit_steps_from_file(StepDefDir),
	gen_def_steps(AllSteps, StepDefDir, ExsitSteps, []).

gen_def_steps([], _StepDefDir,_ExsitSteps, Acc) -> Acc;
gen_def_steps([H|T], StepDefDir, ExsitSteps, Acc) ->
	% io:format("~p~n", [ExsitSteps]),
    {obj, [_,_,{_, KeyWd}, {_, Text}]} = H,
    case is_step_exist(KeyWd, Text, ExsitSteps) of
    	no_exsit_step ->
			{NewStepRe, DefStep}=gen_default_step_def(H),
			file:write_file(StepDefDir, DefStep, [append]),
			NewExsitSteps=update_exist_step(ExsitSteps, NewStepRe),
			gen_def_steps(T, StepDefDir, NewExsitSteps, Acc ++ DefStep);
		_-> gen_def_steps(T, StepDefDir, ExsitSteps, Acc)
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_all_exsit_steps_from_file(FileDir) ->	
	case file:read_file(FileDir) of
		{ok, Bin} -> get_all_exsit_steps(Bin);
		_->[]
	end.

is_str_match_re(InputStr, MatchPattenRE) ->
	{ok,MP}=re:compile(MatchPattenRE),
	re:run(InputStr, MP, [{capture, [1], list}, global]).
is_str_match_re_by_index(InputStr, MatchPattenRE) ->
	{ok,MP}=re:compile(MatchPattenRE),
	Res=re:run(InputStr, MP, [{capture, [1], index}, global]),
	% io:format("----------------~p~n", [Res]),
	NewRes=case Res of
		{match, Data} ->  {match,[{S, string:substr(InputStr,S+1,L)} || [{S,L}]<-Data]};
		_ -> Res
	end,
	NewRes.
get_all_exsit_steps(InputStr) ->
	AllStepType = [{key_given, "/\\* *Given (.*)\\*/"},
	               {key_when,  "/\\* *When (.*)\\*/"},
	               {key_then,  "/\\* *Then (.*)\\*/"},
	               {key_and,   "/\\* *And (.*)\\*/"},
	               {key_but,   "/\\* *But (.*)\\*/"}],
	[{K, is_str_match_re(InputStr, P)} || {K, P} <- AllStepType].

update_exist_step(ExsitSteps, NewStepRe) ->
	NewStep = get_all_exsit_steps(NewStepRe),
	add_new_step_to_exist_step(ExsitSteps,NewStep,[]).

add_new_step_to_exist_step([], [], Acc) -> Acc;
add_new_step_to_exist_step([E_H|E_T], [N_H|N_T], Acc) ->
	NewAcc=Acc ++ gen_new_ele(E_H, N_H),
	add_new_step_to_exist_step(E_T, N_T, NewAcc).
gen_new_ele({K, nomatch}, {K, nomatch}) -> [{K, nomatch}];
gen_new_ele(E, {_K, nomatch}) -> [E];
gen_new_ele({_K, nomatch}, N) -> [N];
gen_new_ele({K, {match, EL}}, {K, {match, NL}}) -> [{K, {match, EL++NL}}].

get_step_type(_Step, [], []) -> undef_step;
get_step_type(_Step, _REList, [{find_type, Type}]) -> Type;
get_step_type(Step, [{Type, Re}|T], _Res) ->
	case is_str_match_re(Step, Re) of
		{match, _} ->
			Val = [{find_type, Type}];
		_ ->
			Val = []
	end,
	get_step_type(Step, T, Val).

is_step_exist(_StepKeyWord, _Step, []) -> no_exsit_step;
is_step_exist(StepKeyWord, Step, ExsitSteps) -> 
	KeyReList = [{key_given, " *Given *"},
	             {key_when, " *When *"},
	             {key_then, " *Then *"},
	             {key_and, " *And *"},
	             {key_but, " *But *"}],
	StepType = get_step_type(StepKeyWord, KeyReList, []),
	Res = check_one_step_exsit(StepType, Step, ExsitSteps),
	Res.

check_one_step_exsit(undef_step, _Step, _ExsitSteps) ->
	no_exsit_step;
check_one_step_exsit(StepType, Step, ExsitSteps) ->
	Res = lists:keyfind(StepType, 1, ExsitSteps),
	case Res of
		{StepType, {match, ExistStepsOfThisType}} ->
			do_check_one_step_exsit(Step, ExistStepsOfThisType, []);
		_ -> no_exsit_step
	end.

do_check_one_step_exsit(_Step, [], []) -> no_exsit_step;
do_check_one_step_exsit(_Step, _, find) -> exsit_step;
do_check_one_step_exsit(Step, [[H]|T], Res) -> 
	% io:format("++++++++++++++++++++++++++++++++++++++++++++++++~s  ~s~n", [Step, H]),
	case is_str_match_re(Step, H) of
		{match, _} -> do_check_one_step_exsit(Step, T, find);
		_ -> do_check_one_step_exsit(Step, T, Res)
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_default_step_def(Step) ->
	% io:format("~p~n", [Step]),
	{obj, [_,_,{_, KeyWd}, {_, Text}]} = Step,
	KeyWdText=binary_to_list(KeyWd) ++ binary_to_list(Text),
	SrcPara=get_step_func_para(KeyWdText),
	% io:format("=====~p~n",[SrcPara]),

	StepNote = gen_step_note(KeyWdText, SrcPara),
	% io:format("~s~n", [StepNote]),

	ReplaceChar = "(" ++ get_replace_chars(SrcPara, []) ++ " |,)",
	% io:format("~p-----~p~n", [KeyWdText, ReplaceChar]),
	StepFuncName = "void " ++ string:to_lower(re:replace(KeyWdText,ReplaceChar,"_",[{return,list}, global])),
	% io:format("~s~n", [StepFuncName]),
	StepFuncPara = gen_fuc_para(SrcPara),
	StepFuncBody = " {\n    // Write code here that turns the phrase above into concrete actions\n    assert(0)\n}\n",
	% io:format("~s~n", [StepFuncName ++ StepFuncPara ++ StepFuncBody]),
	{StepNote, StepNote ++ StepFuncName ++ StepFuncPara ++ StepFuncBody}.

%%%%%%%%%%%%%%%%%%%% 从step中提取 <> int float string类型的参数
get_step_func_para(Step) ->
	StepParaTypeRes = [{outline_para, "( *<\\w+> *)"},
	                   {int_para,    "( +\\d+ *)"},
	                   {float_para,  "( +\\d+\\.\\d+ *)"},
	                   {string_para, "( +\"\\w+\\+*\\** *\\w*\" *)"}],
	do_get_step_para(Step, StepParaTypeRes, []).

do_get_step_para(_Step, [], []) -> no_para;
do_get_step_para(_Step, [], Res) -> 
	SortRes=lists:sort(Res),
	[{K,P} || {_Id, {K,P}} <- SortRes];
do_get_step_para(Step, [{K,V}|T], Res) ->
	R=is_str_match_re_by_index(Step, V), 
	NewR=case R of
		{match,Para} -> Res ++ [{Id, {K, P}} || {Id, P} <- Para];
		_ -> Res
	end,
	do_get_step_para(Step,T,NewR).

%%%%%%%%%%%%%%%%%%%% 产生step C函数的注释的正则表达式
gen_step_note(KeyWdText, SrcPara) ->
	% io:format("~p---~p~n",[KeyWdText, SrcPara]),
	NewKwt = do_gen_step_note(SrcPara, KeyWdText),
	% io:format("===============================~p~n",[NewKwt]),
	"\n/*" ++ NewKwt ++"*/\n".

do_gen_step_note(no_para, Acc) -> Acc;
do_gen_step_note([], Acc) -> Acc;

do_gen_step_note([{string_para, V}|T], Acc) ->
	V1=re:replace(V , "(\\+)", "\\\\+", [{return,list}, global]),
	V2=re:replace(V1 , "(\\*)", "\\\\*", [{return,list}, global]),
	RPL = "(\"" ++ "\\\\w+" ++ "\\\\+*" ++ "\\\\**" ++" *"++ "\\\\w*" ++ "\")",
	NewV=string:strip(V2, both, $ ),
	NewAcc=re:replace(Acc, NewV, RPL,[{return,list}, global]),
	% io:format("ppppppppppppppppppppp~p ~p~n", [Acc, NewAcc]),
	do_gen_step_note(T, NewAcc);

do_gen_step_note([{int_para, V}|T], Acc) -> 
	RPL = "(\\\\d+)",
	NewV=string:strip(V, both, $ ),
	NewAcc=re:replace(Acc, NewV, RPL,[{return,list}, global]),
	do_gen_step_note(T, NewAcc);
do_gen_step_note([{float_para, V}|T], Acc) -> 
	RPL = "(\\\\d+\\\\.+\\\\d+)",
	NewV=string:strip(V, both, $ ),
	NewAcc=re:replace(Acc, NewV, RPL,[{return,list}, global]),
	do_gen_step_note(T, NewAcc);
do_gen_step_note([{_K, _V}|T], Acc) -> do_gen_step_note(T, Acc).

%%%%%%%%%%%%%%%%%%%% 产生函数名的时候需要将 参数替换成空格
get_replace_chars(no_para, _) -> "";
get_replace_chars([], Res) -> Res;
get_replace_chars([{_K,V}|T], Res) ->
	V1=re:replace(V , "(\\+)", "\\\\+", [{return,list}, global]),
	V2=re:replace(V1 , "(\\*)", "\\\\*", [{return,list}, global]),
	get_replace_chars(T, Res ++ V2 ++ "|").

%%%%%%%%%%%%%%%%%%%% 从step 的C函数的函数的入参
gen_fuc_para(no_para) -> "(void)";
gen_fuc_para(SrcPara) -> 
	Paras = [gen_one_para(I, length(SrcPara), lists:nth(I, SrcPara)) || I<-lists:seq(1, length(SrcPara))],
	"(" ++ Paras ++ ")".

gen_one_para(Id, Len, {outline_para, V}) -> 
	Res=re:split(V,"[<>]",[{return,list},trim]),
	case Len =:= Id of
		true -> "int " ++ lists:nth(2, Res);
	    _ -> "int " ++ lists:nth(2, Res) ++", "
	end;	
gen_one_para(Id, Len, {int_para, _V}) -> 
	case Len =:= Id of
		true -> "int para" ++ integer_to_list(Id);
	    _ -> "int para" ++ integer_to_list(Id) ++ ", "
	end;
gen_one_para(Id, Len, {float_para, _V}) -> 
	case Len =:= Id of
		true -> "float para" ++ integer_to_list(Id);
		_ -> "float para" ++ integer_to_list(Id) ++ ", "
	end;
gen_one_para(Id, Len, {string_para, _V}) -> 
	case Len =:= Id of
		true -> "char para" ++ integer_to_list(Id) ++ "[100]";
		_ -> "char para" ++ integer_to_list(Id) ++ "[100]" ++ ", "
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_step_is_def() ->
	FileDir = "./../../features/step_definitions/add_steps.c",
	ExsitSteps = get_all_exsit_steps_from_file(FileDir),
	Step1 = "the input num1 2 num2 3 ",
	Step2 = "the calculator is add run ",
	Step3 = "the output should be 4 ",
	Step4 = "the output should be \"1+2\" cc",
	R1 = is_step_exist("Given ", Step1, ExsitSteps),
	R2 = is_step_exist("When ", Step2, ExsitSteps),
	R3 = is_step_exist("Then ", Step3, ExsitSteps),
	R4 = is_step_exist("Then ", Step4, ExsitSteps),
    {exsit_step, exsit_step, exsit_step, exsit_step} = {R1, R2, R3, R4},
    {match,[{16," 1 "},{26," 2 "}]} = is_str_match_re_by_index("I intput one num 1 two num 2 dd", "( \\d+ )"),
    ok.

test_get_step_func_para() ->
	StepOutLine = "Give I enter <para1> in the computer",
	StepString = "Give I enter \"para1\" in the computer",
	StepInt = "Give I enter 3 in the computer",
	StepFloat = "Give I enter 3.345 in the computer",
	StepMul = "Give I a 3.345 b 2.3 c <eeee> d \"para1\" e 4 f j",
	[{outline_para, " <para1> "}] = get_step_func_para(StepOutLine),
	[{string_para,  " \"para1\" "}] = get_step_func_para(StepString),
	[{int_para,     " 3 "}] = get_step_func_para(StepInt),
	[{float_para,   " 3.345 "}] = get_step_func_para(StepFloat),	
	[{float_para,   " 3.345 "},
	 {float_para,   " 2.3 "},
	 {outline_para, " <eeee> "},
	 {string_para,  " \"para1\" "},
	 {int_para,     " 4 "}] = get_step_func_para(StepMul),
	ok.

test_gen_default_step_def() ->
	StepOutLine1 = 	{obj,[{"type",<<"Step">>},
      					 {"location",{obj,[{"line",8},{"column",5}]}},
                         {"keyword",<<"Given ">>},
                         {"text",<<"I have entered <input_1> into the calculator">>}]},
	gen_default_step_def(StepOutLine1),

	StepOutLine2 = 	{obj,[{"type",<<"Step">>},
      					 {"location",{obj,[{"line",8},{"column",5}]}},
                         {"keyword",<<"Given ">>},
                         {"text",<<"I a 3.345 b 2.3 c <eeee> d \"para1\" e 4 ">>}]},
	gen_default_step_def(StepOutLine2),

	StepOutLine3 = 	{obj,[{"type",<<"Step">>},
      					 {"location",{obj,[{"line",8},{"column",5}]}},
                         {"keyword",<<"Given ">>},
                         {"text",<<"I press button add">>}]},
	gen_default_step_def(StepOutLine3),
	ok.

test() ->
	test_step_is_def(),
	% test_get_step_func_para(),
	% test_gen_default_step_def(),
	ok.
	