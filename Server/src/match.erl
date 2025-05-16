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
        {_, _, _, {Spawn1, _, _, _, _}} = Player1,
        {_, _, _, {Spawn2, _, _, _, _}} = Player2,
        Spawn = {Spawn1, Spawn2},
        Spawn = {Spawn1, Spawn2},
        loop(Players,erlang:monotonic_time(millisecond), Spawn,0)
      end
  end.

loop(Players, StartTime,Spawn,LClock) ->
  Duration = 5000,
  Tickrate = 10,
  AccelX = 0.25,
  AccelY = 0.25,
  MaxVel = 5.0,
  MinPx = 50,
  MinPy = 100,
  MaxPx = 750,
  MaxPy = 700,
  {Spawn1, Spawn2} = Spawn,

  [{User1, Lv1, SPid1, {P1, V1, A1, Pj1, Pt1}},
    {User2, Lv2, SPid2, {P2, V2, A2, Pj2, Pt2}}] = Players,

  receive
    {move, User, Key, Pid} ->
      case {User, Pid} of
        {User1, SPid1} ->
          NewA1 = update_accel(Key,{AccelX, AccelY}),
          loop([{User1, Lv1, SPid1, {P1, V1, NewA1, Pj1, Pt1}},
            {User2, Lv2, SPid2, {P2, V2, A2, Pj2, Pt2}}], StartTime, Spawn,LClock);
        {User2, SPid2} ->
          NewA2 = update_accel(Key,{AccelX, AccelY}),
          loop([{User1, Lv1, SPid1, {P1, V1, A1, Pj1, Pt1}},
            {User2, Lv2, SPid2, {P2, V2, NewA2, Pj2, Pt2}}], StartTime, Spawn,LClock);
        _ ->
          Pid ! {error, "Invalid user"},
          loop(Players, StartTime, Spawn,LClock)
      end;
    {shoot,User, Dirx, Diry, Pid} ->
      case {User, Pid} of
        {User1, SPid1} ->
          NewPj1 = shootprojectile(Pj1, Dirx, Diry,LClock,P1),
          loop([{User1, Lv1, SPid1, {P1, V1, A1, NewPj1, Pt1}},
            {User2, Lv2, SPid2, {P2, V2, A2, Pj2, Pt2}}], StartTime, Spawn,LClock);
        {User2, SPid2} ->
          NewPj2 = shootprojectile(Pj2, Dirx, Diry,LClock,P2),
          loop([{User1, Lv1, SPid1, {P1, V1, A1, Pj1, Pt1}},
            {User2, Lv2, SPid2, {P2, V2, A2, NewPj2, Pt2}}], StartTime, Spawn,LClock);
        _ ->
          Pid ! {error, "Invalid user"},
          loop(Players, StartTime, Spawn,LClock)
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
          {TempPt2,{NewP1,NewV1,NewA1}} = update_Pos_Vel_Col(P1, V1, A1, Pt2, MaxVel,{MinPx, MinPy},{ MaxPx, MaxPy}, Spawn1),
          {TempPt1,{NewP2,NewV2,NewA2}} = update_Pos_Vel_Col(P2, V2, A2, Pt1, MaxVel,{MinPx, MinPy},{ MaxPx, MaxPy}, Spawn2),
          TempPj1 = update_projectiles(Pj1),
          TempPj2 = update_projectiles(Pj2),
          %% Check for projectiles collisions
          {Speed,Cooldown,LastShoot1, PjList1} = TempPj1,
          {_,_,LastShoot2, PjList2} = TempPj2,
          {NewPjList1,_, NewPt1,_,_} = check_pj_collistions({PjList1, P2, TempPt1,{MinPx, MinPy},{MaxPx, MaxPy}}),
          {NewPjList2,_, NewPt2,_,_} = check_pj_collistions({PjList2, P1, TempPt2,{MinPx, MinPy},{MaxPx, MaxPy}}),
          NewPj1 = lists:flatten(NewPjList1),
          NewPj2 = lists:flatten(NewPjList2),
          NewPlayers = [
            {User1, Lv1, SPid1, {NewP1, NewV1, NewA1, {Speed,Cooldown,LastShoot1,NewPj1}, NewPt1}},
            {User2, Lv2, SPid2, {NewP2, NewV2, NewA2, {Speed,Cooldown,LastShoot2,NewPj2}, NewPt2}}
          ],
          SPid1 ! {update, {NewP1, NewP2, Pt1, Pt2, NewPj1, NewPj2, Clock}},
          SPid2 ! {update, {NewP2, NewP1, Pt2, Pt1, NewPj2, NewPj1, Clock}},
          timer:send_after(Tickrate, self(), tick),
          loop(NewPlayers, StartTime, Spawn,Clock)
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

shootprojectile({Speed, Cooldown, LastShootTime, Projectiles}, TargetX, TargetY, LClock, {Px, Py}) ->
  if LClock - LastShootTime >= Cooldown ->
    %% Calculate direction from player to target
    Dx = TargetX - Px,
    Dy = TargetY - Py,
    Length = math:sqrt(Dx * Dx + Dy * Dy),
    case Length > 0 of
      true ->
        NormalizedDirx = Dx / Length,
        NormalizedDiry = Dy / Length,
        %%io:format("NormalizedDirx: ~p, NormalizedDiry: ~p~n", [NormalizedDirx, NormalizedDiry]),
        NewProjectile = {NormalizedDirx, NormalizedDiry, Px, Py},
        NewProjectiles = [NewProjectile | Projectiles],
        {Speed, Cooldown, LClock, NewProjectiles};
      false ->
        %% Avoid division by zero
        {Speed, Cooldown, LastShootTime, Projectiles}
    end;
    true ->
      {Speed, Cooldown, LastShootTime, Projectiles}
  end.

update_projectiles({Speed, Cooldown, LastShootTime, Projectiles}) ->
  %% Update the projectiles based on their speed and direction
  NewProjectiles = lists:map(fun({Dirx, Diry, Px, Py}) ->
    %% Update the position of the projectile
    NewPx = Px + Dirx * Speed,
    NewPy = Py + Diry * Speed,
    {Dirx, Diry, NewPx, NewPy}
  end, Projectiles),
  %% Check for collisions with the players and walls
  %% Return the updated projectile state
  {Speed, Cooldown, LastShootTime, NewProjectiles}.


check_pj_collistions({[Projectile | Projectiles], P, Pt, MinP, MaxP}) ->
  {_, _, Px, Py} = Projectile,
  {Px1, Py1} = P,
  {MinPx, MinPy} = MinP,
  {MaxPx, MaxPy} = MaxP,
  DistanceSq = (Px - Px1) * (Px - Px1) + (Py - Py1) * (Py - Py1),

  case DistanceSq < 25 * 25 of
    true ->
      %% Collision with player
      check_pj_collistions({Projectiles, P, Pt + 1, MinP, MaxP});
    false ->
      %% Check collision with walls
      case (Px - 5 < MinPx) orelse (Px + 5 > MaxPx) orelse
        (Py - 5 < MinPy) orelse (Py + 5 > MaxPy) of
        true ->
          %% Projectile hits wall – skip it
          check_pj_collistions({Projectiles, P, Pt, MinP, MaxP});
        false ->
          %% Keep projectile and continue recursion
          {RemainingProjectiles, P2, FinalPt, MinP2, MaxP2} =
            check_pj_collistions({Projectiles, P, Pt, MinP, MaxP}),
          {[Projectile | RemainingProjectiles], P2, FinalPt, MinP2, MaxP2}
      end
  end;

check_pj_collistions({[], P, Pt, MinP, MaxP}) ->
  {[], P, Pt, MinP, MaxP}.