-module(matchmaker).

%% API
-export([start/0]).

start() ->
  Rid = 0,
  Rooms = #{},
  Waiting = [],
  Pid = spawn(fun() -> loop(Rooms, Rid, Waiting) end),
  register(?MODULE, Pid).

loop(Rooms, Rid, Waiting) ->
  receive
    {SPid, {find_match, User, Lv}} ->
      io:format("User ~p with level ~p is looking for a match~n", [User, Lv]),
      case lists:member({User, Lv, SPid}, Waiting) of
        true ->
          SPid ! already_in_queue,
          loop(Rooms, Rid, Waiting);
        false ->
          case lists:filter(fun({_, Lv2, _}) -> abs(Lv2 - Lv) =< 1 end, Waiting) of
            [Match | _] ->
              {User2, Lv2, SPid2} = Match,
              P1 = {100,400}, %% Player 1 position
              P2 = {700,400}, %% Player 2 position
              V = {0, 0}, %% Velocity
              A = {0, 0}, %% Acceleration
              Ps = 5, %% Projectiles Speed
              Pi = 5, %% Projectiles Interval
              Pt = 0, %% Points
              NewRoom = {Rid, [{User, Lv, SPid, {P1, V, A, Ps, Pi, Pt}}, {User2, Lv2, SPid2, {P2, V, A, Ps, Pi, Pt}}]},
              NewRooms = maps:put(Rid, NewRoom, Rooms),
              NewWaiting = lists:delete(Match, Waiting),
              io:format("Test.~n"),
              MatchPid = match:start(NewRoom),
              io:format("Match found with Pid: ~p~n", [MatchPid]),
              SPid ! {match_found, MatchPid},
              SPid2 ! {match_found, MatchPid},
              loop(NewRooms, Rid + 1, NewWaiting);
            [] ->
              NewWaiting = [{User, Lv, SPid} | Waiting],
              SPid ! waiting,
              loop(Rooms, Rid, NewWaiting)
          end
      end
  end.

