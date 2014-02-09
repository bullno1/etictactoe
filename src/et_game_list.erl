-module(et_game_list).
-export([init/3]).
-export([allowed_methods/2, content_types_accepted/2, resource_exists/2]).
-export([create_game/2]).

init(_, _Req, []) -> {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"OPTIONS">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
	{[{'*', create_game}], Req, State}.

resource_exists(Req, State) -> {false, Req, State}.

create_game(Req, State) ->
	GameId = random_id(),
	{ok, _} = et_game_sup:new_game(GameId),
	{{true, <<"/games/", GameId/binary>>}, Req, State}.

random_id() ->
	base64url:encode(crypto:hash(sha, term_to_binary({node(), make_ref()}))).
