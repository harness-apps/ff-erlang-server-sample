-module(sample_threeflag).
-export([run_percentage_rollout_evaluations/0]).

-define(PARENT_FLAG, <<"parentflag3">>).
-define(NEW_PARENT_FLAG, <<"rightchild3">>).

run_percentage_rollout_evaluations() ->
  io:format("Running 20k evaluations on flag ~p~n", [?PARENT_FLAG]),

  % Initial evaluations on the parent flag
  ParentFlagResults = evaluate_and_store_targets(?PARENT_FLAG, #{}, 0),

  io:format("Running 20k evaluations on flag ~p~n", [?NEW_PARENT_FLAG]),

  % Evaluations on the new parent flag
  NewParentFlagResults = evaluate_and_store_targets(?NEW_PARENT_FLAG, #{}, 0),

  % Check differences between the two results
  DifferingTargets = compare_results(ParentFlagResults, NewParentFlagResults),

  CountDifferingTargets = length(DifferingTargets),

  io:format("Differing targets count: ~p~n", [CountDifferingTargets]).

evaluate_and_store_targets(_, Targets, 5000) ->
  Targets;

evaluate_and_store_targets(FlagIdentifier, Targets, Counter) ->
  TargetIdentifierNumber = integer_to_binary(Counter),
  BaseTarget = #{
    identifier => <<"target", TargetIdentifierNumber/binary>>,
    name => <<"targetname", TargetIdentifierNumber/binary>>,
    attributes => #{host => <<"true">>}
  },

  Experience = cfclient:string_variation(FlagIdentifier, BaseTarget, <<"excluded">>),
  NewTargets = maps:put(maps:get(identifier, BaseTarget), Experience, Targets),

  evaluate_and_store_targets(FlagIdentifier, NewTargets, Counter + 1).

compare_results(ParentFlagResults, NewParentFlagResults) ->
  compare_results(maps:to_list(ParentFlagResults), NewParentFlagResults, []).

compare_results([], _NewParentFlagResults, Acc) ->
  Acc;

compare_results([{Key, Value} | Rest], NewParentFlagResults, Acc) ->
  case maps:get(Key, NewParentFlagResults, undefined) of
    Value ->
      % The result is the same for this key in both maps
      compare_results(Rest, NewParentFlagResults, Acc);
    _ ->
      % The result is different for this key in both maps
      compare_results(Rest, NewParentFlagResults, [Key | Acc])
  end.
