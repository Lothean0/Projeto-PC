-module(matchmaker).

%% API
-export([start/0]).

start() ->
  Rid = 0,
  Rooms = #{},
  Waiting = [],
  Pid = spawn(fun() -> loop(Rooms, Rid, Waiting) end),
  register(?MODULE, Pid),
  Pid.



loop(Rooms, Rid, Waiting) ->
  receive
    {SPid, {find_match, User, Lv}} ->
      case lists:member({User, Lv, SPid}, Waiting) of
        true ->
          SPid ! already_in_queue,
          loop(Rooms, Rid, Waiting);
        false ->
          case lists:filter(fun({_, Lv2, _}) -> abs(Lv2 - Lv) =< 1 end, Waiting) of
            [Match | _] ->
              {User2, Lv2, SPid2} = Match,
              NewRoom = {Rid, [{User, Lv, SPid}, {User2, Lv2, SPid2}]},
              NewRooms = maps:put(Rid, NewRoom, Rooms),
              NewWaiting = lists:delete(Match, Waiting),
              SPid ! {match_found, Rid},
              SPid2 ! {match_found, Rid},
              loop(NewRooms, Rid + 1, NewWaiting);
            [] ->
              NewWaiting = [{User, Lv, SPid} | Waiting],
              SPid ! waiting,
              loop(Rooms, Rid, NewWaiting)
          end
      end;
    {SPid, {decide, Id, Data}} ->
      io:format("Decide: ~p, ~p~n", [Id, Data]),
      case maps:find(Id, Rooms) of
        {ok, {_RoomId, Players}} ->
          case lists:keyfind(SPid, 3, Players) of
            {_User, _Lv, SPid} ->
              %% Handle the decision (win/lose)
              case Data of
                "win" ->
                  SPid ! win,
                  loop(Rooms, Rid, Waiting);
                "lose" ->
                  SPid ! lose,
                  loop(Rooms, Rid, Waiting);
                _ ->
                  SPid ! {error, "Invalid decision"},
                  loop(Rooms, Rid, Waiting)
              end;
            false ->
              SPid ! {error, "You are not in this room"},
              loop(Rooms, Rid, Waiting)
          end;
        error ->
          SPid ! {error, "Room not found"},
          loop(Rooms, Rid, Waiting)
      end
  end.

