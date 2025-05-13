-module(match).

%% API
-export([start/1]).

start(Room) ->
  io:format("Test2.~n"),
  {_Rid, Players} = Room,
  Pid = spawn(fun() -> connector(Players) end),
  Pid.

connector(Players) ->
  receive
    {connect, User1, Pid1} ->
      receive
      {connect, User2, Pid2} ->
        Pid1 ! {connected, User1},
        Pid2 ! {connected, User2},
        self() ! tick,
        loop(Players,erlang:monotonic_time(millisecond))
      end
  end.

loop(Players, StartTime) ->
  Duration = 10000,
  Tickrate = 10,
  AccelX = 0.1,
  AccelY = 0.1,
  MaxAccel = 1.0,
  MaxVel = 5.0,

  [{User1, Lv1, SPid1, {P1, V1, A1, Ps1, Pi1, Pt1}},
    {User2, Lv2, SPid2, {P2, V2, A2, Ps2, Pi2, Pt2}}] = Players,

  receive
    {move, User, Key, Pid} ->
      case {User, Pid} of
        {User1, SPid1} ->
          NewA1 = update_accel(Key, A1, {AccelX, AccelY}, MaxAccel),
          loop([{User1, Lv1, SPid1, {P1, V1, NewA1, Ps1, Pi1, Pt1}},
            {User2, Lv2, SPid2, {P2, V2, A2, Ps2, Pi2, Pt2}}], StartTime);

        {User2, SPid2} ->
          NewA2 = update_accel(Key, A2, {AccelX, AccelY}, MaxAccel),
          loop([{User1, Lv1, SPid1, {P1, V1, A1, Ps1, Pi1, Pt1}},
            {User2, Lv2, SPid2, {P2, V2, NewA2, Ps2, Pi2, Pt2}}], StartTime);

        _ ->
          Pid ! {error, "Invalid user"},
          loop(Players, StartTime)
      end;

    tick ->
      Clock = erlang:monotonic_time(millisecond) - StartTime,
      io:format("Clock: ~p, StartTime: ~p, Duration: ~p~n", [Clock, StartTime, Duration]),
      if Clock > Duration ->
        if Pt1 > Pt2 ->
          SPid1 ! win,
          SPid2 ! lose;
          Pt2 > Pt1 ->
            SPid1 ! lose,
            SPid2 ! win;
          true ->
            SPid1 ! draw,
            SPid2 ! draw
        end;
        true ->
          {NewP1, NewV1} = update_position_and_speed(P1, V1, A1,MaxVel),
          {NewP2, NewV2} = update_position_and_speed(P2, V2, A2,MaxVel),
          NewPlayers = [
            {User1, Lv1, SPid1, {NewP1, NewV1, A1, Ps1, Pi1, Pt1}},
            {User2, Lv2, SPid2, {NewP2, NewV2, A2, Ps2, Pi2, Pt2}}
          ],
          SPid1 ! {update, {NewP1, NewP2, Pt1, Pt2, Clock}},
          SPid2 ! {update, {NewP2, NewP1, Pt2, Pt1, Clock}},
          timer:send_after(Tickrate, self(), tick),
          loop(NewPlayers, StartTime)
      end
  end.

update_position_and_speed({Px, Py}, {Vx, Vy}, {Ax, Ay},MaxVel) ->

  %% Update speed
  TempVx = Vx + Ax,
  TempVy = Vy + Ay,

  %% Clamp speed
  ClampedVx = max(min(TempVx, MaxVel), -MaxVel),
  ClampedVy = max(min(TempVy, MaxVel), -MaxVel),

  %% Update position
  NewPx = Px + ClampedVx,
  NewPy = Py + ClampedVy,

  %%io:format("Ax ~p |Ay ~p~n", [Ax, Ay]),
  %%io:format("Vx ~p |Vy ~p~n", [ClampedVx, ClampedVy]),
  %%io:format("Px ~p |Py ~p~n", [NewPx, NewPy]),

  {{NewPx, NewPy}, {ClampedVx, ClampedVy}}.

update_accel(Key, {Ax, Ay}, {AccelX, AccelY}, Max) ->
  case Key of
    "w" ->
      if
        Ay < 0 ->
          {Ax, clamp(Ay - AccelY, -Max, Max)};
        true ->
          {Ax, clamp(0 - AccelY, -Max, Max)}
      end;
    "s" ->
      if
        Ay > 0 ->
          {Ax, clamp(Ay + AccelY, -Max, Max)};
        true ->
          {Ax, clamp(0 + AccelY, -Max, Max)}
      end;
    "a" ->
      if
        Ax < 0 ->
          {clamp(Ax - AccelX, -Max, Max), Ay};
        true ->
          {clamp(0 - AccelX, -Max, Max), Ay}
      end;
    "d" ->
      if
        Ax > 0 ->
          {clamp(Ax + AccelX, -Max, Max), Ay};
        true ->
          {clamp(0 + AccelX, -Max, Max), Ay}
      end;
    _   -> {Ax, Ay}
  end.

clamp(Value, Min, _Max) when Value < Min -> Min;
clamp(Value, _Min, Max) when Value > Max -> Max;
clamp(Value, _, _) -> Value.