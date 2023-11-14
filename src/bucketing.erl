-module(bucketing).

-export([run_percentage_rollout_evaluations/1]).

-define(LOG_FORMAT, "Final Percentage Values (rounded to 2 decimal places):\n"
"excluded1: ~p (~p%%)\n"
"control: ~p (~p%%)\n"
"variant: ~p (~p%%)\n").

run_percentage_rollout_evaluations(FlagIdentifier) ->
  io:format("Running 100k evaluations on flag ~p~n", [FlagIdentifier]),
  {RolloutVariant1Count, RolloutVariant2Count, RolloutVariant3Count} =
    evaluate_100k_unique_targets(FlagIdentifier, {0, 0, 0}, 0),

  RolloutVariant1Percentage = round(RolloutVariant1Count / 100000 * 100 * 100) / 100,
  RolloutVariant2Percentage = round(RolloutVariant2Count / 100000 * 100 * 100) / 100,
  RolloutVariant3Percentage = round(RolloutVariant3Count / 100000 * 100 * 100) / 100,

  io:format("Final Variation Evaluation Counts: Variant 1: ~p, Variant 2: ~p, Variant 3: ~p~n",
    [RolloutVariant1Count, RolloutVariant2Count, RolloutVariant3Count]),
  io:format(?LOG_FORMAT,
    [RolloutVariant1Count, RolloutVariant1Percentage,
      RolloutVariant2Count, RolloutVariant2Percentage,
      RolloutVariant3Count, RolloutVariant3Percentage]).

evaluate_100k_unique_targets(_, {Variation1Counter, Variation2Counter, Variation3Counter}, 100000) ->
  {Variation1Counter, Variation2Counter, Variation3Counter};
evaluate_100k_unique_targets(FlagIdentifier, {Variation1Counter, Variation2Counter, Variation3Counter}, Accu_in) ->
  Counter = Accu_in + 1,
  TargetIdentifierNumber = integer_to_list(Counter),
  DynamicTarget = #{identifier => "target" ++ TargetIdentifierNumber,
    name => "targetname" ++ TargetIdentifierNumber,
    attributes => #{host => true}},
  case cfclient:string_variation(FlagIdentifier, DynamicTarget, "default") of
    <<"excluded">> ->
      evaluate_100k_unique_targets(
        FlagIdentifier,
        {Variation1Counter + 1, Variation2Counter, Variation3Counter},
        Counter
      );
    <<"control">> ->
      evaluate_100k_unique_targets(
        FlagIdentifier,
        {Variation1Counter, Variation2Counter + 1, Variation3Counter},
        Counter
      );
    <<"variant">> ->
      evaluate_100k_unique_targets(
        FlagIdentifier,
        {Variation1Counter, Variation2Counter, Variation3Counter + 1},
        Counter
      )
  end.
