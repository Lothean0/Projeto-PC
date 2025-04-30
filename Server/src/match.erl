-module(match).

%% API
-export([start/1]).

start(Room) ->
  io:format("Test2.~n"),
  {Rid, Players} = Room,
  Pid = spawn(fun() -> connector(Rid, Players, []) end),
  Pid.

connector(Rid, Players, Connected) ->
  receive
    {connect, User, Pid} ->
      [{User1, _Lv1, SPid1, {_P1, _V1, _A1, _Ps1, _Pi1, _Pt1}},
        {User2, _Lv2, SPid2, {_P2, _V2, _A2, _Ps2, _Pi2, _Pt2}}] = Players,
      io:format("Test3.~n"),
      if
        User == User1 orelse User == User2 ->
          %% Send the current state to the connected user
          SPid = if User == User1 -> SPid1; true -> SPid2 end,
          SPid ! {connected, User},
          NewConnected = [{User, SPid} | Connected],
          if
            length(NewConnected) == 2 ->
              self() ! tick,
              loop(Players);
            true ->
              connector(Rid, Players, NewConnected)
          end;
        true ->
          %% User not in the match
          Pid ! {error, "User not in this match"},
          connector(Rid, Players, Connected)
      end
  end.

loop(Players) ->
  [{User1, _Lv1, SPid1, {P1, V1, A1, Ps1, Pi1, Pt1}},
    {User2, _Lv2, SPid2, {P2, V2, A2, Ps2, Pi2, Pt2}}] = Players,
  receive
    {move, User, Ax, Ay, Pid} ->
      io:format("User: ~p, ~nUser1: ~p, ~nUser2: ~p, ~nAx: ~p, ~nAy: ~p, ~nPid: ~p~nSPid1: ~p, ~nSPid2: ~p~n", [User, User1, User2, Ax, Ay, Pid, SPid1, SPid2]),
      case User of
        User1 ->
          if Pid == SPid1 ->
            NewA1 = {Ax, Ay},
            loop([{User1, _Lv1, SPid1, {P1, V1, NewA1, Ps1, Pi1, Pt1}},
              {User2, _Lv2, SPid2, {P2, V2, A2, Ps2, Pi2, Pt2}}]);
            true ->
              %% Invalid user
              Pid ! {error, "Invalid user"},
              loop(Players)
          end;
        User2 ->
          if Pid == SPid2 ->
            %% Update acceleration for User2
            NewA2 = {Ax, Ay},
            loop([{User1, _Lv1, SPid1, {P1, V1, A1, Ps1, Pi1, Pt1}},
              {User2, _Lv2, SPid2, {P2, V2, NewA2, Ps2, Pi2, Pt2}}]);
            true ->
              %% Invalid user
              Pid ! {error, "Invalid user"},
              loop(Players)
          end;
        _ ->
          %% Invalid user
          loop(Players)
      end;
    tick ->
      %% Update position and speed for both players
      {NewP1, NewV1} = update_position_and_speed(P1, V1, A1),
      {NewP2, NewV2} = update_position_and_speed(P2, V2, A2),
      NewPlayers = [
        {User1, _Lv1, SPid1, {NewP1, NewV1, A1, Ps1, Pi1, Pt1}},
        {User2, _Lv2, SPid2, {NewP2, NewV2, A2, Ps2, Pi2, Pt2}}
      ],
      %% Send updated positions to players
      SPid1 ! {update, NewP1},
      SPid2 ! {update, NewP2},
      %% Schedule the next tick
      timer:send_after(10000, self(), tick),
      loop(NewPlayers)
  end.

update_position_and_speed({Px, Py}, {Vx, Vy}, {Ax, Ay}) ->
  %% Update speed
  NewVx = Vx + Ax,
  NewVy = Vy + Ay,
  %% Update position
  NewPx = Px + NewVx,
  NewPy = Py + NewVy,
  {{NewPx, NewPy}, {NewVx, NewVy}}.