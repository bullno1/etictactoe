-module(et_game).
-behaviour(gen_fsm).
-export([start_link/1, exists/1, join/1, play/3, status/1]).
-export([waiting/2, waiting/3,
         playing/3,
         finished/2, finished/3]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).
-record(state, {
	board = et_board:new(),
	x_player,
	o_player,
	turn = x
}).
-define(WHERE(Name), {via, gproc, {n, l, {?MODULE, Name}}}).

-type role() :: x | o.
-type outcome() :: {victory, role()} | draw.
-type board() :: tuple(9).

-spec start_link(binary()) -> {ok, pid()}.
start_link(Name) ->
	gen_fsm:start_link({via, gproc, {n, l, {?MODULE, Name}}}, ?MODULE, [], []).

-spec join(binary()) -> {ok, role()} | {error, full | finished}.
join(Name) ->
	gen_fsm:sync_send_event(?WHERE(Name), join).

-spec play(binary(), pos_integer(), pos_integer()) -> {error, Reason} | continue | {finished, outcome()} when
	   Reason :: not_started | invalid_move | wrong_turn | finished.
play(Name, X, Y) ->
	gen_fsm:sync_send_event(?WHERE(Name), {play, X, Y}).

-spec status(binary()) -> waiting | {playing, board(), role()} | {finished, outcome()}.
status(Name) ->
	gen_fsm:sync_send_event(?WHERE(Name), status).

-spec exists(binary()) -> boolean().
exists(Name) ->
	gproc:lookup_pids({n, l, {?MODULE, Name}}) =/= [].

% gen_fsm

init([]) ->
	{ok, waiting, #state{}, 5000}.

terminate(_Reason, _StateName, _State) ->
	gproc:goodbye(),
	ok.

waiting(timeout, StateData) -> {stop, normal, StateData}.

waiting(join, {XPid, _}, #state{x_player = undefined} = StateData) ->
	{reply, {ok, x}, waiting, StateData#state{x_player = XPid}};
waiting(join, {OPid, _}, #state{x_player = XPid, o_player = undefined} = StateData) ->
	XPid ! {et_game, start, true},
	OPid ! {et_game, start, false},
	{reply, {ok, o}, playing, StateData#state{o_player = OPid}};
waiting({play, _, _}, _From, StateData) ->
	{reply, {error, not_started}, waiting, StateData};
waiting(status, _From, StateData) ->
	{reply, waiting, waiting, StateData}.

playing(join, _From, StateData) ->
	{reply, {error, full}, playing, StateData};
playing({play, X, Y}, {Player, _}, #state{x_player = XPlayer,
                                          o_player = OPlayer,
                                          turn = Turn} = StateData) ->
	if
		X < 1; X > 3; Y < 1, Y > 3 ->
			{reply, {error, invalid_move}, playing, StateData};
		Turn =:= x, Player =:= XPlayer ->
			process_move(x, X, Y, StateData);
		Turn =:= o, Player =:= OPlayer ->
			process_move(o, X, Y, StateData);
		true ->
			{reply, {error, wrong_turn}, playing, StateData}
	end;
playing(status, _From, #state{turn = Turn, board = Board} = StateData) ->
	{reply, {playing, Turn, Board}, playing, StateData}.

process_move(Who, X, Y, #state{board = Board} = StateData) ->
	case et_board:get(X, Y, Board) =:= undefined of
		true ->
			execute_move(Who, X, Y, StateData);
		false ->
			{reply, {error, invalid_move}, playing, StateData}
	end.

execute_move(Who, X, Y, #state{board = Board} = StateData) ->
	Board2 = et_board:play(Who, X, Y, Board),
	StateData2 = StateData#state{board = Board2},
	Result = et_board:check(Board2),
	EndGame =
		case Result of
			{victory, _Winner} -> true;
			draw -> true;
			undecided -> false
		end,
	case EndGame of
		true ->
			send_to_other({et_game, move, Who, X, Y, Result}, StateData2),
			{reply, Result, finished, StateData2, 3000};
		false ->
			send_to_other({et_game, move, Who, X, Y, continue}, StateData2),
			{reply, continue, playing, next_turn(StateData2)}
	end.

next_turn(#state{turn = x} = StateData) -> StateData#state{turn = o};
next_turn(#state{turn = o} = StateData) -> StateData#state{turn = x}.

finished(timeout, StateData) -> {stop, normal, StateData}.

finished(join, _From, StateData) ->
	{reply, {error, finished}, finished, StateData, 3000};
finished({play, _, _}, _From, StateData) ->
	{reply, {error, finished}, finished, StateData, 3000};
finished(status, _From, #state{board = Board} = StateData) ->
	{reply, {finished, et_board:check(Board)}, finished, StateData, 3000}.

send_to_other(Msg, #state{x_player = Pid, turn = o}) -> Pid ! Msg;
send_to_other(Msg, #state{o_player = Pid, turn = x}) -> Pid ! Msg.

handle_event(_Event, _StateName, StateData) ->
	{stop, unexpected, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
	{stop, unexpected, StateData}.

handle_info(_Info, _StateName, StateData) ->
	{stop, unexpected, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.
