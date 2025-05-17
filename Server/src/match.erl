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
        [Player1, Player2,CD,SP] = Players,
        {_, _, _, {Spawn1, _, _, _, _}} = Player1,
        {_, _, _, {Spawn2, _, _, _, _}} = Player2,
        Spawn = {Spawn1, Spawn2},
        Spawn = {Spawn1, Spawn2},
        %%{CD,SP}
        Modifiers = {[],[]},
        loop([Player1,Player2],erlang:monotonic_time(millisecond), Spawn,0, Modifiers,{CD,SP})
      end
  end.

loop(Players, StartTime,Spawn,LClock,Modifiers,CD_SP) ->
  Duration = 120000,
  Tickrate = 10,
  AccelX = 0.25,
  AccelY = 0.25,
  MaxVel = 5.0,
  MinPx = 50,
  MinPy = 100,
  MaxPx = 750,
  MaxPy = 700,
  MaxMod = 5,
  {Spawn1, Spawn2} = Spawn,

  [{User1, Lv1, SPid1, {P1, V1, A1, Pj1, Pt1}},
    {User2, Lv2, SPid2, {P2, V2, A2, Pj2, Pt2}}] = Players,

  receive
    end_match ->
      SPid1 ! end_match,
      SPid2 ! end_match,
      exit(normal);
    {move, User, Key, Pid} ->
      case {User, Pid} of
        {User1, SPid1} ->
          NewA1 = update_accel(Key,{AccelX, AccelY}),
          loop([{User1, Lv1, SPid1, {P1, V1, NewA1, Pj1, Pt1}},
            {User2, Lv2, SPid2, {P2, V2, A2, Pj2, Pt2}}], StartTime, Spawn,LClock,Modifiers,CD_SP);
        {User2, SPid2} ->
          NewA2 = update_accel(Key,{AccelX, AccelY}),
          loop([{User1, Lv1, SPid1, {P1, V1, A1, Pj1, Pt1}},
            {User2, Lv2, SPid2, {P2, V2, NewA2, Pj2, Pt2}}], StartTime, Spawn,LClock,Modifiers,CD_SP);
        _ ->
          Pid ! {error, "Invalid user"},
          loop(Players, StartTime, Spawn,LClock,Modifiers,CD_SP)
      end;
    {shoot,User, Dirx, Diry, Pid} ->
      case {User, Pid} of
        {User1, SPid1} ->
          NewPj1 = shootprojectile(Pj1, Dirx, Diry,LClock,P1),
          loop([{User1, Lv1, SPid1, {P1, V1, A1, NewPj1, Pt1}},
            {User2, Lv2, SPid2, {P2, V2, A2, Pj2, Pt2}}], StartTime, Spawn,LClock,Modifiers,CD_SP);
        {User2, SPid2} ->
          NewPj2 = shootprojectile(Pj2, Dirx, Diry,LClock,P2),
          loop([{User1, Lv1, SPid1, {P1, V1, A1, Pj1, Pt1}},
            {User2, Lv2, SPid2, {P2, V2, A2, NewPj2, Pt2}}], StartTime, Spawn,LClock,Modifiers,CD_SP);
        _ ->
          Pid ! {error, "Invalid user"},
          loop(Players, StartTime, Spawn,LClock,Modifiers,CD_SP)
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
          {CD,SP} = CD_SP,
          NewModifiers=generate_random_modifier(Modifiers,MaxMod,{MinPx, MinPy},{MaxPx, MaxPy}),
          {TempPt2,{TempP1,TempP2,TempV1,TempV2,TempA1,TempA2,Temp1Pj1},NewModifiers2} = update(P1,P2, V1, V2, A1, A2, Pt2, MaxVel,{MinPx, MinPy},{ MaxPx, MaxPy}, Spawn1,Spawn2, Pj1,NewModifiers),
          {TempPt1,{NewP2,NewP1,NewV2,NewV1,NewA2,NewA1,Temp1Pj2},NewModifiers3} = update(TempP2,TempP1, TempV2,TempV1, TempA2,TempA1, Pt1, MaxVel,{MinPx, MinPy},{ MaxPx, MaxPy}, Spawn2,Spawn1, Pj2,NewModifiers2),
          %%io:format("NewModifiers: ~p~n", [NewModifiers3]),
          TempPj1 = update_projectiles(Temp1Pj1),
          TempPj2 = update_projectiles(Temp1Pj2),
          %% Check for projectiles collisions
          {_,_,_, PjList1} = TempPj1,
          {_,_,_, PjList2} = TempPj2,
          {NewPjList1,_, NewPt1,_,_} = check_pj_collisions({PjList1, P2, TempPt1,{MinPx, MinPy},{MaxPx, MaxPy}}),
          {NewPjList2,_, NewPt2,_,_} = check_pj_collisions({PjList2, P1, TempPt2,{MinPx, MinPy},{MaxPx, MaxPy}}),
          NewPj1 = lists:flatten(NewPjList1),
          NewPj2 = lists:flatten(NewPjList2),
          %% Update the cooldown and speed of projectiles
          {Speed1, Cooldown1, LastShoot1, _} = update_cd_sp(TempPj1, {CD,SP}),
          {Speed2, Cooldown2, LastShoot2, _} = update_cd_sp(TempPj2, {CD,SP}),
          NewPlayers = [
            {User1, Lv1, SPid1, {NewP1, NewV1, NewA1, {Speed1,Cooldown1,LastShoot1,NewPj1}, NewPt1}},
            {User2, Lv2, SPid2, {NewP2, NewV2, NewA2, {Speed2,Cooldown2,LastShoot2,NewPj2}, NewPt2}}
          ],
          SPid1 ! {update, {NewP1, NewP2, Pt1, Pt2, NewPj1, NewPj2, Clock,NewModifiers3}},
          SPid2 ! {update, {NewP2, NewP1, Pt2, Pt1, NewPj2, NewPj1, Clock,NewModifiers3}},
          timer:send_after(Tickrate, self(), tick),
          loop(NewPlayers, StartTime, Spawn,Clock,NewModifiers3,CD_SP)
      end
  end.

update({Px1, Py1},{Px2,Py2}, {Vx1, Vy1},{Vx2,Vy2},{Ax1, Ay1},{Ax2,Ay2}, Pt, MaxVel, MinP,MaxP, Spawn1,Spawn2, Pj,Modifiers) ->
  {MinPx, MinPy} = MinP,
  {MaxPx, MaxPy} = MaxP,
  {Spawnx1, Spawny1} = Spawn1,
  {Spawnx2, Spawny2} = Spawn2,
  {Speed, Cooldown, LastShootTime, Projectiles} = Pj,

  %% Update speed
  TempVx = Vx1 + Ax1,
  TempVy = Vy1 + Ay1,

  %% Clamp speed
  ClampedVx = max(min(TempVx, MaxVel), -MaxVel),
  ClampedVy = max(min(TempVy, MaxVel), -MaxVel),

  %% Update position
  NewPx = Px1 + ClampedVx,
  NewPy = Py1 + ClampedVy,

  %% Check for collision with walls
  case (NewPx - 25 < MinPx) orelse (NewPx + 25 > MaxPx) orelse
    (NewPy - 25 < MinPy) orelse (NewPy + 25 > MaxPy) of
    true ->
      %% Collision detected: increment Pt, reset position, velocity, and acceleration
      {Pt + 2, {{Spawnx1, Spawny1}, {Spawnx2,Spawny2}, {0, 0}, {0, 0},{0, 0}, {0, 0}, Pj},Modifiers};
    false ->
      {NewCDMods, NewSPMods, NewCD, NewSP} = check_Mods_collisions({Modifiers, {NewPx, NewPy}, Speed, Cooldown}),
      {Pt, {{NewPx, NewPy},{Px2,Py2}, {ClampedVx, ClampedVy}, {Vx2,Vy2}, {Ax1, Ay1}, {Ax2,Ay2},{NewSP,NewCD, LastShootTime, Projectiles}}, {NewCDMods, NewSPMods}}
  end.

update_accel(Key,{AccelX, AccelY}) ->
  %%io:format("Key: ~p~n", [Key]),
  case Key of
    "w" -> {0, -AccelY};
    "s" -> {0, AccelY};
    "a" -> {-AccelX, 0};
    "d" -> {AccelX, 0};
    _ -> {0, 0}
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
        NewProjectile = {NormalizedDirx, NormalizedDiry, Px, Py,Speed},
        NewProjectiles = [NewProjectile | Projectiles],
        {Speed, Cooldown, LClock, NewProjectiles};
      false ->
        %% Avoid division by zero
        {Speed, Cooldown, LastShootTime, Projectiles}
    end;
    true ->
      {Speed, Cooldown, LastShootTime, Projectiles}
  end.

update_projectiles({_Speed, Cooldown, LastShootTime, Projectiles}) ->
  %% Update the projectiles based on their speed and direction
  NewProjectiles = lists:map(fun({Dirx, Diry, Px, Py,Speed}) ->
    %% Update the position of the projectile
    NewPx = Px + Dirx * Speed,
    NewPy = Py + Diry * Speed,
    {Dirx, Diry, NewPx, NewPy, Speed}
  end, Projectiles),
  %% Check for collisions with the players and walls
  %% Return the updated projectile state
  {_Speed, Cooldown, LastShootTime, NewProjectiles}.


check_pj_collisions({[Projectile | Projectiles], P, Pt, MinP, MaxP}) ->
  {_, _, Px, Py, _Speed} = Projectile, %% Include Speed in the pattern
  {Px1, Py1} = P,
  {MinPx, MinPy} = MinP,
  {MaxPx, MaxPy} = MaxP,
  DistanceSq = (Px - Px1) * (Px - Px1) + (Py - Py1) * (Py - Py1),

  case DistanceSq < 25 * 25 of
    true ->
      %% Collision with player
      check_pj_collisions({Projectiles, P, Pt + 1, MinP, MaxP});
    false ->
      %% Check collision with walls
      case (Px - 5 < MinPx) orelse (Px + 5 > MaxPx) orelse
        (Py - 5 < MinPy) orelse (Py + 5 > MaxPy) of
        true ->
          %% Projectile hits wall – skip it
          check_pj_collisions({Projectiles, P, Pt, MinP, MaxP});
        false ->
          %% Keep projectile and continue recursion
          {RemainingProjectiles, P2, FinalPt, MinP2, MaxP2} =
            check_pj_collisions({Projectiles, P, Pt, MinP, MaxP}),
          {[Projectile | RemainingProjectiles], P2, FinalPt, MinP2, MaxP2}
      end
  end;
check_pj_collisions({[], P, Pt, MinP, MaxP}) ->
  {[], P, Pt, MinP, MaxP}.

check_Mods_collisions({{CDMods, SPMods}, P, SP, CD}) ->
  {NewCDMods,_, NewCD} = check_CDMods_collisions([],CDMods, P, CD),
  {NewSPMods,_, NewSP} = check_SPMods_collisions([],SPMods, P, SP),
  {NewCDMods, NewSPMods, NewCD, NewSP}.

check_CDMods_collisions(Acumul, [CDMod | Rest], P, CD) ->
  {Px, Py} = P,
  {{X, Y}, Value} = CDMod,
  Distance = math:sqrt((Px - X) * (Px - X) + (Py - Y) * (Py - Y)),
  case Distance < 25 of
    true ->
      %% Collision detected: apply modifier and remove it from the list
      %%io:format("Collision detected: ~p~n", [CDMod]),
      NewCD = max(CD + Value, 0),
      check_CDMods_collisions(Acumul, Rest, P, NewCD);
    false ->
      %% No collision: keep the modifier and continue recursion
      check_CDMods_collisions([CDMod | Acumul], Rest, P, CD)
  end;
check_CDMods_collisions(Acumul, [], _P, CD) ->
  {Acumul, [], CD}.

check_SPMods_collisions(Acumul, [SPMod | Rest], P, SP) ->
  {Px, Py} = P,
  {{X, Y}, Value} = SPMod,
  Distance = math:sqrt((Px - X) * (Px - X) + (Py - Y) * (Py - Y)),
  case Distance < 25 of
    true ->
      %% Collision detected: apply modifier and remove it from the list
      %%io:format("Collision detected: ~p~n", [SPMod]),
      NewSP = max(SP + Value, 0),
      check_SPMods_collisions(Acumul, Rest, P, NewSP);
    false ->
      %% No collision: keep the modifier and continue recursion
      check_SPMods_collisions([SPMod | Acumul], Rest, P, SP)
  end;
check_SPMods_collisions(Acumul, [], _P, SP) ->
  {Acumul, [], SP}.

%% Update the cooldown and speed of projectiles for each tick
update_cd_sp({Speed, Cooldown, LastShootTime, Projectiles}, {CD, SP}) ->
  %% Change this Values to better ones latter
  SpeedAdjustmentRate = 0.001,
  CooldownAdjustmentRate = 0.1,

  %% Gradually adjust Speed towards SP
  NewSpeed =
    case Speed < SP of
      true -> min(Speed + SpeedAdjustmentRate, SP);
      false -> max(Speed - SpeedAdjustmentRate, SP)
    end,

  %% Gradually adjust Cooldown towards CD
  NewCooldown =
    case Cooldown < CD of
      true -> min(Cooldown + CooldownAdjustmentRate, CD);
      false -> max(Cooldown - CooldownAdjustmentRate, CD)
    end,

  %% Return the updated state
  {NewSpeed, NewCooldown, LastShootTime, Projectiles}.

%% Generate a random Modifier and add it to the Modifiers
generate_random_modifier(Modifiers, MaxMod, MinP, MaxP) ->
  {CDMods, SPMods} = Modifiers,
  {MinPx, MinPy} = MinP,
  {MaxPx, MaxPy} = MaxP,
  %% Change this Values to better ones latter
  CDChange = 100,
  SPChange = 2.5,
  case rand:uniform(1000) =< 2 of
    true ->
      %% Generate a random modifier
      RandomValue = rand:uniform(2),
      case RandomValue of
        1 when length(CDMods) < MaxMod ->
          RandomSign = rand:uniform(2),
          NewCDmod = {generate_random_position({MinPx, MinPy}, {MaxPx, MaxPy}),
            if RandomSign =:= 1 -> CDChange; true -> -CDChange end},
          {[NewCDmod | CDMods], SPMods};
        2 when length(SPMods) < MaxMod ->
          RandomSign = rand:uniform(2),
          NewSPmod = {generate_random_position({MinPx, MinPy}, {MaxPx, MaxPy}),
            if RandomSign =:= 1 -> SPChange; true -> -SPChange end},
          {CDMods, [NewSPmod | SPMods]};
        _ ->
          Modifiers %% Handle cases where no new modifier is added
      end;
    false ->
      Modifiers
  end.

generate_random_position({MinPx, MinPy}, {MaxPx, MaxPy}) ->
  SafeMinPx = MinPx + 11,
  SafeMinPy = MinPy + 11,
  SafeMaxPx = MaxPx - 11,
  SafeMaxPy = MaxPy - 11,

  %% Generate random positions within the safe boundaries
  X = SafeMinPx + rand:uniform(SafeMaxPx - SafeMinPx + 1) - 1,
  Y = SafeMinPy + rand:uniform(SafeMaxPy - SafeMinPy + 1) - 1,
  {X, Y}.

