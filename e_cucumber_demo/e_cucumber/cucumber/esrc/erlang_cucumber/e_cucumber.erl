-module(e_cucumber).
-include("e_cucumber_utility.hrl").
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run() ->
	StartT = now(),
	test_outline(),
	test_simple(),
	test_multi(),
	test_background(),
	test_data_list(),
	test_tic_tac_toe(),
	EndT = now(),
	ExecuteTime = timer:now_diff(EndT, StartT),
	print_cucumber_excute_result(ExecuteTime),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_simple() ->
	%% mmetadata dir
    GherkinDocJsonDir = "./../../features/simple_feature_doc.json",
    StepDefDir = "./../../features/step_definitions/simple_feature_steps.c",

    %% scan 
	{AllSteps,AllParseRes} = scan_gherkin_doc_json:run(GherkinDocJsonDir),	
	NotExistStep=scan_step:run({AllSteps,AllParseRes}, StepDefDir),
	print_not_exsit_step(NotExistStep),	
	ok.

test_outline() ->
    %% mmetadata dir
    GherkinDocJsonDir = "./../../features/outline_feature_doc.json",
    StepDefDir = "./../../features/step_definitions/outline_feature_steps.c",

    %% scan 
	{AllSteps,AllParseRes} = scan_gherkin_doc_json:run(GherkinDocJsonDir),
	NotExistStep=scan_step:run({AllSteps,AllParseRes}, StepDefDir),
	print_not_exsit_step(NotExistStep),	
	ok.

test_multi() ->
	%% mmetadata dir
    GherkinDocJsonDir = "./../../features/change_pin_feature_doc.json",
    StepDefDir = "./../../features/step_definitions/change_pin_feature_doc.c",

    %% scan 
	{AllSteps,AllParseRes} = scan_gherkin_doc_json:run(GherkinDocJsonDir),	
	NotExistStep=scan_step:run({AllSteps,AllParseRes}, StepDefDir),
	print_not_exsit_step(NotExistStep),	
	ok.

test_background() ->
	%% mmetadata dir
    GherkinDocJsonDir = "./../../features/change_pin_bg_feature_doc.json",
    StepDefDir = "./../../features/step_definitions/change_pin_bg_feature_doc.c",

    %% scan 
	{AllSteps,AllParseRes} = scan_gherkin_doc_json:run(GherkinDocJsonDir),	
	NotExistStep=scan_step:run({AllSteps,AllParseRes}, StepDefDir),
	print_not_exsit_step(NotExistStep),	
	ok.
test_data_list() ->
	%% mmetadata dir
    GherkinDocJsonDir = "./../../features/data_list_feature_doc.json",
    StepDefDir = "./../../features/step_definitions/data_list_feature_doc.c",

    %% scan 
	{AllSteps,AllParseRes} = scan_gherkin_doc_json:run(GherkinDocJsonDir),	
	NotExistStep=scan_step:run({AllSteps,AllParseRes}, StepDefDir),
	print_not_exsit_step(NotExistStep),	
	ok.

test_tic_tac_toe() ->
	%% mmetadata dir
    GherkinDocJsonDir = "./../../features/tic_tac_toe_feature_doc.json",
    StepDefDir = "./../../features/step_definitions/tic_tac_toe_feature_doc.c",

    %% scan 
	{AllSteps,AllParseRes} = scan_gherkin_doc_json:run(GherkinDocJsonDir),	
	NotExistStep=scan_step:run({AllSteps,AllParseRes}, StepDefDir),
	print_not_exsit_step(NotExistStep),	
	ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%
print_cucumber_excute_result(ExecuteTime) ->
	io:format ("~n3 senarios (3 undifined)~n"),
	io:format ("12 steps (12 undifined)~n"),
	io:format ("~pmin--~psec--~pms~n", [ExecuteTime div (60*1000*1000), 
		                                ExecuteTime div (1000*1000), 
		                                (ExecuteTime rem (1000*1000)) div (1000)]).

%%%%%%%%%%%%%
print_not_exsit_step([]) -> ok;
print_not_exsit_step(NotExistStep) ->
    io:format("~nEcucumber has automatically implemented these not exist step definitions ~n"),
	io:format("~n~s~s~s~n", [?CONSOLE_COLOR_YELLOW, NotExistStep, ?CONSOLE_COLOR_NORMAL]),
	ok.