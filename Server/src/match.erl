-module(match).

%% API
-export([start/1]).

start(Room) ->
  %%:format("Test2.~n"),
  {_Rid, Players} = Room,
  Pid = spawn(fun() -> connector(Players) end),
  Pid.

connector(Players) ->
  receive
    {connect, _User1, _Pid1} ->
      receive
      {connect, _User2, _Pid2} ->
        self() ! tick,
        %% get the initial position of the players
        [Player1, Player2] = Players,
        {_, _, _, {Spawn1, _, _, _, _, _}} = Player1,
        {_, _, _, {Spawn2, _, _, _, _, _}} = Player2,
        Spawn = {Spawn1, Spawn2},
        Spawn = {Spawn1, Spawn2},
        loop(Players,erlang:monotonic_time(millisecond), Spawn)
      end
  end.

loop(Players, StartTime,Spawn) ->
  Duration = 120000,
  Tickrate = 10,
  AccelX = 0.25,
  AccelY = 0.25,
  MaxVel = 5.0,
  MinPx = 0,
  MinPy = 0,
  MaxPx = 800,
  MaxPy = 800,
  {Spawn1, Spawn2} = Spawn,

  [{User1, Lv1, SPid1, {P1, V1, A1, Ps1, Pi1, Pt1}},
    {User2, Lv2, SPid2, {P2, V2, A2, Ps2, Pi2, Pt2}}] = Players,

  receive
    {move, User, Key, Pid} ->
      case {User, Pid} of
        {User1, SPid1} ->
          NewA1 = update_accel(Key,{AccelX, AccelY}),
          loop([{User1, Lv1, SPid1, {P1, V1, NewA1, Ps1, Pi1, Pt1}},
            {User2, Lv2, SPid2, {P2, V2, A2, Ps2, Pi2, Pt2}}], StartTime, Spawn);
        {User2, SPid2} ->
          NewA2 = update_accel(Key,{AccelX, AccelY}),
          loop([{User1, Lv1, SPid1, {P1, V1, A1, Ps1, Pi1, Pt1}},
            {User2, Lv2, SPid2, {P2, V2, NewA2, Ps2, Pi2, Pt2}}], StartTime, Spawn);

        _ ->
          Pid ! {error, "Invalid user"},
          loop(Players, StartTime, Spawn)
      end;

    tick ->
      Clock = erlang:monotonic_time(millisecond) - StartTime,
      %%io:format("Clock: ~p, StartTime: ~p, Duration: ~p~n", [Clock, StartTime, Duration]),
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
          {NewPt2,{NewP1,NewV1,NewA1}} = update_Pos_Vel_Col(P1, V1, A1, Pt2, MaxVel,{MinPx, MinPy},{ MaxPx, MaxPy}, Spawn1),
          {NewPt1,{NewP2,NewV2,NewA2}} = update_Pos_Vel_Col(P2, V2, A2, Pt1, MaxVel,{MinPx, MinPy},{ MaxPx, MaxPy}, Spawn2),
          NewPlayers = [
            {User1, Lv1, SPid1, {NewP1, NewV1, NewA1, Ps1, Pi1, NewPt1}},
            {User2, Lv2, SPid2, {NewP2, NewV2, NewA2, Ps2, Pi2, NewPt2}}
          ],
          SPid1 ! {update, {NewP1, NewP2, Pt1, Pt2, Clock}},
          SPid2 ! {update, {NewP2, NewP1, Pt2, Pt1, Clock}},
          timer:send_after(Tickrate, self(), tick),
          loop(NewPlayers, StartTime, Spawn)
      end
  end.

update_Pos_Vel_Col({Px, Py}, {Vx, Vy}, {Ax, Ay}, Pt, MaxVel, MinP,MaxP, Spawn) ->
  {MinPx, MinPy} = MinP,
  {MaxPx, MaxPy} = MaxP,
  {Spawnx, Spawny} = Spawn,

  %% Update speed
  TempVx = Vx + Ax,
  TempVy = Vy + Ay,

  %% Clamp speed
  ClampedVx = max(min(TempVx, MaxVel), -MaxVel),
  ClampedVy = max(min(TempVy, MaxVel), -MaxVel),

  %% Update position
  NewPx = Px + ClampedVx,
  NewPy = Py + ClampedVy,

  %% Check for collision with walls
  case (NewPx - 25 < MinPx) orelse (NewPx + 25 > MaxPx) orelse
    (NewPy - 25 < MinPy) orelse (NewPy + 25 > MaxPy) of
    true ->
      %% Collision detected: increment Pt, reset position, velocity, and acceleration
      {Pt + 1, {{Spawnx, Spawny}, {0, 0}, {0, 0}}};
    false ->
      %% No collision: return updated position and velocity
      {Pt, {{NewPx, NewPy}, {ClampedVx, ClampedVy}, {Ax, Ay}}}
  end.

update_accel(Key,{AccelX, AccelY}) ->
  io:format("Key: ~p~n", [Key]),
  case Key of
    "w" -> {0, -AccelY};
    "s" -> {0, AccelY};
    "a" -> {-AccelX, 0};
    "d" -> {AccelX, 0};
    "space" -> {0, 0};
    _ -> {AccelX, AccelY}
  end.