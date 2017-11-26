-module(scan_gherkin_pickle_json).
-include("e_cucumber_utility.hrl").
-compile(export_all).
-import(e_cucumber_uitility, [get_loc/1, print_line_no/2, print_n_space/1]).

run(StepStatus, PickleObjs)->
	case StepStatus of
		some_step_not_implement -> ok;
		_->parse_pickle_json(PickleObjs)
	end,
	ok.
parse_pickle_json(PickleObjs) -> 
	% io:format("parse_pickle_json: ~n~p~n", [PickleObjs]),
	ok.