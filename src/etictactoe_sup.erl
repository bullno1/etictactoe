-module(etictactoe_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	ChildSpecs = [
		{et_game_sup, {et_game_sup, start_link, []}, permanent, 5000, supervisor, [et_game_sup]}
	],
	{ok, {{one_for_one, 5, 60}, ChildSpecs}}.
