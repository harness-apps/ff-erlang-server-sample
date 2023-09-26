-module(sample_multievals).

-export([run_percentage_rollout_evaluations/1]).

-define(LOG_FORMAT, "Final Percentage Values (rounded to 2 decimal places):\n"
"excluded1: ~p (~p%%)\n"
"control: ~p (~p%%)\n"
"variant: ~p (~p%%)\n").

run_percentage_rollout_evaluations(FlagIdentifier) ->
  io:format("Running 100k evaluations on flag ~p~n", [FlagIdentifier]),
  InitialState = {0, 0, 0},
  {RolloutVariant1Count, RolloutVariant2Count, RolloutVariant3Count} =
    lists:foldl(fun(Target, Counts) ->
      evaluate_target(FlagIdentifier, Target, Counts)
                end, InitialState, lists:seq(1, 100000)),

  RolloutVariant1Percentage = round(RolloutVariant1Count / 100000 * 100 * 100) / 100,
  RolloutVariant2Percentage = round(RolloutVariant2Count / 100000 * 100 * 100) / 100,
  RolloutVariant3Percentage = round(RolloutVariant3Count / 100000 * 100 * 100) / 100,

  io:format("Final Variation Evaluation Counts: Variant 1: ~p, Variant 2: ~p, Variant 3: ~p~n",
    [RolloutVariant1Count, RolloutVariant2Count, RolloutVariant3Count]),
  io:format(?LOG_FORMAT,
    [RolloutVariant1Count, RolloutVariant1Percentage,
      RolloutVariant2Count, RolloutVariant2Percentage,
      RolloutVariant3Count, RolloutVariant3Percentage]).

evaluate_target(FlagIdentifier, Target, {V1, V2, V3}) ->
  TargetIdentifierNumber = integer_to_list(Target),
  DynamicTarget = #{
    identifier => "target" ++ TargetIdentifierNumber,
    name => "targetname" ++ TargetIdentifierNumber,
    attributes => #{host => true}
  },
  case cfclient:string_variation(FlagIdentifier, DynamicTarget, "default") of
    <<"excluded">> -> {V1 + 1, V2, V3};
    <<"control">> -> {V1, V2 + 1, V3};
    <<"variant">> -> {V1, V2, V3 + 1}
  end.
