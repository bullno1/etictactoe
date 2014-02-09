-module(et_game_socket).
-behaviour(cowboy_websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).
-record(state, {
	game_id
}).

init(_, _Req, []) -> {upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
	case cowboy_req:binding(game_id, Req) of
		{undefined, Req2} ->
			{shutdown, Req2};
		{GameId, Req2} ->
			case et_game:exists(GameId) of
				true ->
					{ok, cowboy_req:compact(Req2), #state{game_id = GameId}};
				false ->
					{shutdown, Req2}
			end
	end.

websocket_handle({text, Msg}, Req, State) ->
	handle_client_msg(jsx:decode(Msg), Req, State).

websocket_info(Msg, Req, State) when element(1, Msg) =:= et_game ->
	{reply, encode(erlang:delete_element(1, Msg)), Req, State}.

websocket_terminate(_Reason, _Req, _State) -> ok.

% Private

handle_client_msg(<<"join">>, Req, #state{game_id = GameId} = State) ->
	{reply, encode(et_game:join(GameId)), Req, State};
handle_client_msg([<<"play">>, X, Y], Req, #state{game_id = GameId} = State) ->
	{reply, encode(et_game:play(GameId, X, Y)), Req, State};
handle_client_msg(<<"status">>, Req, #state{game_id = GameId} = State) ->
	{reply, encode(et_game:status(GameId)), Req, State}.

encode(Term) ->
	{text, jsx:encode(jsonify(Term))}.

jsonify(Tuple) when is_tuple(Tuple) ->
	jsonify(tuple_to_list(Tuple));
jsonify(List) when is_list(List) ->
	[jsonify(Element) || Element <- List];
jsonify(Term) when is_number(Term); is_binary(Term); is_boolean(Term) ->
	Term;
jsonify(Atom) when is_atom(Atom) ->
	erlang:atom_to_binary(Atom, utf8).
