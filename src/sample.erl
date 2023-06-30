%%%-------------------------------------------------------------------
%%% @author erowlands
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Feb 2023 19:33
%%%-------------------------------------------------------------------
-module(sample).

%% API
-export([multi_instance_evaluations/0, percentage/0]).

multi_instance_evaluations() ->
  Target = #{
    identifier => "Harness_Target_1",
    name => "HT_1",
    attributes => #{email => <<"demo@harness.io">>}
  },

  %% Instance 1
  Project1Flag = <<"harnessappdemodarkmodeproject1">>,
  Project1Result = cfclient:bool_variation(instance_name_1, Project1Flag, Target, false),
  logger:info("Instance Name 1 : Variation for Flag ~p with Target ~p is: ~p~n",
    [Project1Flag, maps:get(identifier, Target), Project1Result]),

  %% Instance 2
  Project2Flag = <<"harnessappdemodarkmodeproject2">>,
  Project2Result = cfclient:bool_variation(instance_name_2, Project2Flag, Target, false),
  logger:info("Instance name 2 Variation for Flag ~p with Target ~p is: ~p~n",
  [Project2Flag, maps:get(identifier, Target), Project2Result]),

  %% Default instance
  DefaultProjectFlag = <<"harnessappdemodarkmodedefaultproject">>,
  DefaultProjectResult = cfclient:bool_variation(instance_name_2, DefaultProjectFlag, Target, false),
  logger:info("Default instance Variation for Flag ~p with Target ~p is: ~p~n",
    [DefaultProjectFlag, maps:get(identifier, Target), DefaultProjectResult]).

percentage() ->
  Target = #{
    identifier => "frrr",
    name => "frrr",
    attributes => #{ anonymous => <<"true">>, host => <<"s">>}
  },
  Result = cfclient:string_variation("string2", Target, <<"blah">>),
  logger:info("result is: ~p", [Result]).
