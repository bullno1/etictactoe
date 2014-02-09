-module(et_game_sup).
-behaviour(supervisor).
-export([start_link/0, new_game/1]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

new_game(Name) ->
	supervisor:start_child(?MODULE, [Name]).

% supervisor

init([]) ->
	ChildSpecs = [
		{et_game, {et_game, start_link, []}, temporary, 5000, worker, [et_game]}
	],
	{ok, {{simple_one_for_one, 5, 60}, ChildSpecs}}.
