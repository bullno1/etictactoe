-module(etictactoe).
-behaviour(application).
-export([reload_routes/0]).
-export([start/2, stop/1]).

start(_, _) ->
	Dispatch = dispatch(),
	HttpOpts = [
		{env, [{dispatch, Dispatch}]},
		{compress, true}
	],
	{ok, _} = cowboy:start_http(etictactoe_http, 100, [{port, 8080}], HttpOpts),
	etictactoe_sup:start_link().
	
stop(_) ->
	ok.

reload_routes() ->
	cowboy:set_env(etictactoe_http, dispatch, dispatch()).

% Private

dispatch() ->
	cowboy_router:compile([{'_', routes()}]).

routes() ->
	[{"/", cowboy_static, {priv_file, etictactoe, "www/index.html"}}
	,{"/games", et_game_list, []}
	,{"/games/:game_id", cowboy_static, {priv_file, etictactoe, "www/game.html"}}
	,{"/games/:game_id/sock", et_game_socket, []}
	,{"/[...]", cowboy_static, {priv_dir, etictactoe, "www"}}].
