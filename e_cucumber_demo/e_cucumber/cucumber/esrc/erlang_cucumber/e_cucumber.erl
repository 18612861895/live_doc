-module(e_cucumber).
-include("e_cucumber_utility.hrl").
-compile(export_all).
-import(e_cucumber_uitility, [load_json_all_objs/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run() ->
	StartT = now(),
	test_simple(),
	test_multi(),
	test_background(),
	test_outline(),
	test_data_list(),
	test_tic_tac_toe(),
	EndT = now(),
	ExecuteTime = timer:now_diff(EndT, StartT),
	print_cucumber_excute_result(ExecuteTime),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_simple() ->
    GherkinJsonDir = "./../../features/simple_feature.json",
    StepDefDir = "./../../features/step_definitions/simple_feature_steps.c",
	do_run(GherkinJsonDir, StepDefDir),
	ok.

test_multi() ->
    GherkinJsonDir = "./../../features/change_pin_feature.json",
    StepDefDir = "./../../features/step_definitions/change_pin_feature_steps.c",
	do_run(GherkinJsonDir, StepDefDir),
	ok.

test_background() ->
    GherkinJsonDir = "./../../features/change_pin_bg_feature.json",
    StepDefDir = "./../../features/step_definitions/change_pin_bg_feature_steps.c",
    do_run(GherkinJsonDir, StepDefDir),
	ok.

test_outline() ->
    GherkinJsonDir = "./../../features/outline_feature.json",
    StepDefDir = "./../../features/step_definitions/outline_feature_steps.c",
	do_run(GherkinJsonDir, StepDefDir),
	ok.

test_data_list() ->
	GherkinJsonDir = "./../../features/data_list_feature.json",
    StepDefDir = "./../../features/step_definitions/data_list_feature_steps.c",
	do_run(GherkinJsonDir, StepDefDir),
	ok.

test_tic_tac_toe() ->
	GherkinJsonDir = "./../../features/tic_tac_toe_feature.json",
    StepDefDir = "./../../features/step_definitions/tic_tac_toe_feature_steps.c",
	do_run(GherkinJsonDir, StepDefDir),
	ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%
do_run(GherkinJsonDir, StepDefDir) ->
	%% get metadata
    AllObjs=load_json_all_objs(GherkinJsonDir),
    {<<"gherkin-document">>,[DocObj]}=lists:keyfind(<<"gherkin-document">>,1,AllObjs),
    {<<"pickle">>,PickleObjs}=lists:keyfind(<<"pickle">>,1,AllObjs),

    %% scan 
	{AllSteps,AllParseRes} = scan_gherkin_doc_json:run(DocObj),	
	NotExistStep=scan_step:run({AllSteps,AllParseRes}, StepDefDir),
	StepStatus=print_not_exsit_step(NotExistStep),
	scan_gherkin_pickle_json:run(StepStatus, PickleObjs),	
	ok.
%%%%%%%%%%%%%
print_cucumber_excute_result(ExecuteTime) ->
	io:format ("~n3 senarios (3 undifined)~n"),
	io:format ("12 steps (12 undifined)~n"),
	io:format ("~pmin--~psec--~pms~n", [ExecuteTime div (60*1000*1000), 
		                                ExecuteTime div (1000*1000), 
		                                (ExecuteTime rem (1000*1000)) div (1000)]).

%%%%%%%%%%%%%
print_not_exsit_step([]) -> all_step_implement;
print_not_exsit_step(NotExistStep) ->
    io:format("~nEcucumber has automatically implemented these not exist step definitions ~n"),
	io:format("~n~s~s~s~n", [?CONSOLE_COLOR_YELLOW, NotExistStep, ?CONSOLE_COLOR_NORMAL]),
	some_step_not_implement.