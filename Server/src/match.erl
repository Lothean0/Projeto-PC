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

loop(Players,StartTime) ->
  Duration = 10000, %% 2 minutes
  [{User1, Lv1, SPid1, {P1, V1, A1, Ps1, Pi1, Pt1}},
    {User2, Lv2, SPid2, {P2, V2, A2, Ps2, Pi2, Pt2}}] = Players,
  receive
    {move, User, Ax, Ay, Pid} ->
      %%io:format("User: ~p, ~nUser1: ~p, ~nUser2: ~p, ~nAx: ~p, ~nAy: ~p, ~nPid: ~p~nSPid1: ~p, ~nSPid2: ~p~n", [User, User1, User2, Ax, Ay, Pid, SPid1, SPid2]),
      case User of
        User1 ->
          if Pid == SPid1 ->
            NewA1 = {Ax, Ay},
            loop([{User1, Lv1, SPid1, {P1, V1, NewA1, Ps1, Pi1, Pt1}},
              {User2, Lv2, SPid2, {P2, V2, A2, Ps2, Pi2, Pt2}}],StartTime);
            true ->
              %% Invalid user
              Pid ! {error, "Invalid user"},
              loop(Players,StartTime)
          end;
        User2 ->
          if Pid == SPid2 ->
            %% Update acceleration for User2
            NewA2 = {Ax, Ay},
            loop([{User1, Lv1, SPid1, {P1, V1, A1, Ps1, Pi1, Pt1}},
              {User2, Lv2, SPid2, {P2, V2, NewA2, Ps2, Pi2, Pt2}}],StartTime);
            true ->
              %% Invalid user
              Pid ! {error, "Invalid user"},
              loop(Players,StartTime)
          end;
        _ ->
          %% Invalid user
          loop(Players,StartTime)
      end;
    tick ->
      Clock = erlang:monotonic_time(millisecond) - StartTime,
      io:format("Clock: ~p, StartTime: ~p, Duration: ~p~n", [Clock, StartTime, Duration]),
      %% Check if time is up
      if Clock > Duration ->
        %% Time is up, end the match
        if Pt1 > Pt2 ->
          %% User1 wins
          SPid1 ! win,
          SPid2 ! lose;
          Pt2 > Pt1 ->
          %% User2 wins
          SPid1 ! lose,
          SPid2 ! win;
          true ->
          %% Draw
          SPid1 ! draw,
          SPid2 ! draw
        end;
        true ->
          %% Update position and speed for both players
          {NewP1, NewV1} = update_position_and_speed(P1, V1, A1),
          {NewP2, NewV2} = update_position_and_speed(P2, V2, A2),
          NewPlayers = [
            {User1, Lv1, SPid1, {NewP1, NewV1, A1, Ps1, Pi1, Pt1}},
            {User2, Lv2, SPid2, {NewP2, NewV2, A2, Ps2, Pi2, Pt2}}
          ],
          SPid1 ! {update, {NewP1, NewP2,Clock}},
          SPid2 ! {update, {NewP2, NewP1,Clock}},
          %% Schedule the next tick
          timer:send_after(100, self(), tick),
          loop(NewPlayers,StartTime)
      end
  end.

update_position_and_speed({Px, Py}, {Vx, Vy}, {Ax, Ay}) ->
  %% Update speed
  NewVx = Vx + Ax,
  NewVy = Vy + Ay,
  %% Update position
  NewPx = Px + NewVx,
  NewPy = Py + NewVy,
  {{NewPx, NewPy}, {NewVx, NewVy}}.