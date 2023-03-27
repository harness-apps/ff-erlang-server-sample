%%%-------------------------------------------------------------------
%%% @author erowlands
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2023 16:21
%%%-------------------------------------------------------------------
-module(sampleapp_sup).
-author("erowlands").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec start_link(proplists:proplist()) -> supervisor:startlink_ret().
start_link(Args) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init([]) ->
  HarnessProject1Args = application:get_env(harness_project_1_config, cfclient, []),
  HarnessProject2Args = application:get_env(harness_project_2_config, cfclient, []),

  ChildSpec1 = #{id => project1_cfclient_instance, start => {cfclient_instance, start_link, [HarnessProject1Args]}},
  ChildSpec2 = #{id => project2_cfclient_instance, start => {cfclient_instance, start_link, [HarnessProject2Args]}},

  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  {ok, {SupFlags, [ChildSpec1, ChildSpec2]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
