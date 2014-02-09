-module(etictactoe).
-behaviour(application).
-export([reload_routes/0]).
-export([start/2, stop/1]).

start(_, _) ->
	{ok, _} = cowboy:start_http(etictactoe_http, 100, [{port, 8080}], http_opts()),
	etictactoe_sup:start_link().
	
stop(_) ->
	ok.

reload_routes() ->
	ranch:set_protocol_options(etictactoe_http, http_opts()).

% Private

http_opts() ->
	Dispatch = cowboy_router:compile([{'_', routes()}]),
	[{env, [{dispatch, Dispatch}]}].

routes() ->
	[{"/", cowboy_static, {priv_file, etictactoe, "www/index.html"}}
	,{"/games", et_game_list, []}
	,{"/games/:game_id", cowboy_static, {priv_file, etictactoe, "www/game.html"}}
	,{"/games/:game_id/sock", et_game_socket, []}
	,{"/[...]", cowboy_static, {priv_dir, etictactoe, "www"}}].
