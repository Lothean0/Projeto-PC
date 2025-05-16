-module(server).
-export([start/1, stop/1]).



start(Port) ->
  {ok, LSock} = gen_tcp:listen(Port, [{packet, line},
                                                  {reuseaddr, true},
                                                  {exit_on_close, false},
                                                  {ip, any}]),
  io:format("Server started on port ~p~n", [Port]),
  loginManager:start(),
  matchmaker:start(),
  %spawn(fun() -> tick_loop() end),
  spawn(fun() -> acceptor(LSock) end).


stop(Server) ->
  Server ! stop.

acceptor(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(fun() -> acceptor(LSock) end),
  user_logged_out(Sock).

user_logged_out(Sock) ->
  receive
    {tcp_closed, Sock} ->
      %% Handle socket closure
      gen_tcp:close(Sock);
    {tcp, _, Data} ->
      %%io:format("Received data: ~p~n", [Data]),
      case string:tokens(string:trim(Data), " ") of
        ["/cr", User, Pass] ->
          case loginManager:create_account(User, Pass) of
            ok ->
              send_message(Sock, {reply, "Account created successfully"}),
              user_logged_out(Sock);
            user_exists ->
              %% User already exists
              send_message(Sock, {reply, "User already exists"}),
              user_logged_out(Sock)
          end;
        ["/cl", User, Pass] ->
          %% Close account
          case loginManager:close_account(User, Pass) of
            ok ->
              send_message(Sock, {reply, "Account closed successfully"}),
              user_logged_out(Sock);
            _ ->
              send_message(Sock, {reply, "Account closure failed"}),
              user_logged_out(Sock)
          end;
        ["/l", User, Pass] ->
          %% Login
          case loginManager:login(User, Pass, Sock) of
            ok ->
              send_message(Sock, {reply, "Login successful"}),
              user_logged_in(Sock, User);
            _ ->
              send_message(Sock, {reply, "Login failed"}),
              user_logged_out(Sock)
          end;
        ["/ld"] ->
          %% Leaderboard
          case loginManager:leaderboard() of
            [] ->
              send_message(Sock, {reply, "No users found"}),
              user_logged_out(Sock);
            Leaderboard ->
              send_message(Sock, {leaderboard, Leaderboard}),
              user_logged_out(Sock)
          end;
        Data ->
          io:format("Received data: ~p~n", [Data]),
          %% Invalid command
          send_message(Sock, {reply, "Invalid command"}),
          user_logged_out(Sock)
      end
  end.

user_logged_in(Sock, User) ->
  receive
    {tcp_closed, Sock} ->
      %% Handle socket closure
      loginManager:logout(User),
      gen_tcp:close(Sock);
    {tcp, Sock, Data} ->
      io:format("Received data: ~p~n", [Data]),
      %%io:format("Received data: ~p~n", [Data]),
      case string:tokens(string:trim(Data), " ") of
        ["/f"] ->
          %% Send a message to the matchmaker process
          matchmaker ! {self(), {find_match, User, loginManager:check_lv(User)}},
          user_logged_in(Sock, User);
        ["/Lv"] ->
          %% Check level
          case loginManager:check_lv(User) of
            user_not_found ->
              send_message(Sock, {reply, "Failed to check LV"}),
              user_logged_in(Sock, User);
            Lv when is_integer(Lv) ->
              io:format("Level: ~p~n", [Lv]),
              send_message(Sock, {checkLV, Lv}),
              user_logged_in(Sock, User);
            _ ->
              send_message(Sock, {reply, "Unexpected error while checking LV"}),
              user_logged_in(Sock, User)
          end;
        ["/Str"] ->
          %% Check streak
          case loginManager:check_streak(User) of
            user_not_found ->
              send_message(Sock, {reply, "Failed to check streak"}),
              user_logged_in(Sock, User);
            Streak when is_integer(Streak) ->
              io:format("Streak: ~p~n", [Streak]),
              send_message(Sock, {checkStreak, Streak}),
              user_logged_in(Sock, User);
            _ ->
              send_message(Sock, {reply, "Unexpected error while checking streak"}),
              user_logged_in(Sock, User)
          end;
        ["/ld"] ->
          %% Leaderboard
          case loginManager:leaderboard() of
            [] ->
              send_message(Sock, {reply, "No users found"}),
              user_logged_in(Sock, User);
            Leaderboard ->
              io:format("Leaderboard: ~p~n", [Leaderboard]),
              send_message(Sock, {leaderboard, Leaderboard}),
              user_logged_in(Sock, User)
          end;
        ["/q"] ->
          case loginManager:logout(User) of
            ok ->
              send_message(Sock, {reply, "Logged out successfully"}),
              user_logged_out(Sock);
            _ ->
              send_message(Sock, {reply,"Logout failed"}),
              user_logged_in(Sock, User)
          end;
        ["/stop"] ->
          matchmaker ! {self(), {stopsearching, User}},
          user_logged_in(Sock, User);
        _ ->
          send_message(Sock, {reply,"Invalid command"}),
          user_logged_in(Sock, User)
      end;
    already_in_queue ->
      send_message(Sock, {reply,"You are already in the queue for a match."}),
      user_logged_in(Sock, User);
    waiting ->
      send_message(Sock, {reply,"Searching for a match."}),
      user_logged_in(Sock, User);
    {stopedsearching, User} ->
      send_message(Sock, {reply,"Stopped searching for a match."}),
      user_logged_in(Sock, User);
    {match_found, MatchPid} ->
      send_message(Sock, {reply,"Match found!"}),
      MatchPid ! {connect, User, self()},
      %%receive
      %%  connected ->
      match(MatchPid, Sock, User)
      %%end
  end.

match(MatchPid, Sock, User) ->
  receive
    {tcp_closed, Sock} ->
      loginManager:logout(User),
      gen_tcp:close(Sock);
    {tcp, Sock, Data} ->
      %%io:format("Received data: ~p~n", [Data]),
      CleanData = string:trim(Data), %% Trim the input
      case string:tokens(CleanData, " ") of
        ["/m", Key] ->
          %% Send move command to the match process
          MatchPid ! {move, User, Key, self()};
        ["/s", Dirx, Diry] ->
          %% Send shoot command to the match process
          IntDirx = list_to_integer(Dirx),
          IntDiry = list_to_integer(Diry),
          FloatDirx = float(IntDirx),
          FloatDiry = float(IntDiry),
          MatchPid ! {shoot, User, FloatDirx, FloatDiry, self()};
        _ ->
          %% Handle invalid input
          send_message(Sock, {reply,"Invalid command"})
      end,
      match(MatchPid, Sock, User);
    {update, {P1, P2, Pt1, Pt2, Pj1, Pj2, Clock,Modifiers}} ->
      %% Update the position of the player
      send_message(Sock, {gamedata, {P1, P2, Pt1, Pt2, Pj1, Pj2, Clock, Modifiers}}),
      match(MatchPid, Sock, User);
    win ->
      case loginManager:win(User) of
        ok ->
          %%io:format("User ~p won!~n", [User]),
          send_message(Sock, {reply,"You win!"}),
          user_logged_in(Sock, User);
        _ ->
          send_message(Sock, {reply,"Error updating win status"}),
          user_logged_in(Sock, User)
      end,
      user_logged_in(Sock, User);
    lose ->
      case loginManager:lose(User) of
        ok ->
          %%io:format("User ~p lost!~n", [User]),
          send_message(Sock, {reply,"You lose!"}),
          user_logged_in(Sock, User);
        _ ->
          send_message(Sock, {reply,"Error updating lose status"}),
          user_logged_in(Sock, User)
      end,
      user_logged_in(Sock, User);
    draw ->
      send_message(Sock, {reply,"Draw!"}),
      user_logged_in(Sock, User);
    Data ->
      %% Handle other messages
      io:format("Unknown message: ~p~n", [Data]),
      match(MatchPid, Sock, User)
  end.


format_XMl(Data) ->
  case Data of
    {gamedata, {P1, P2, Pt1, Pt2, Pj1, Pj2, Clock, Modifiers}} ->
      {P1x, P1y} = P1,
      {P2x, P2y} = P2,
      %% Format each projectile as an XML element
      Projectiles1 = lists:map(
        fun({_, _, Px, Py, _Speed}) -> %% Include Speed in the pattern
          {projectile, [
            {x, float_to_list(float(Px))},
            {y, float_to_list(float(Py))}
          ], []}
        end, Pj1),

      Projectiles2 = lists:map(
        fun({_, _, Px, Py, _Speed}) -> %% Include Speed in the pattern
          {projectile, [
            {x, float_to_list(float(Px))},
            {y, float_to_list(float(Py))}
          ], []}
        end, Pj2),

      {CDMods, SPMods} = Modifiers,
      CDModsXML = lists:map(
        fun({{Mx, My}, ModValue}) ->
          {modifier, [
            {type, "CD"},
            {x, float_to_list(float(Mx))},
            {y, float_to_list(float(My))},
            {value, float_to_list(float(ModValue))}
          ], []}
        end, CDMods),

      SPModsXML = lists:map(
        fun({{Mx, My}, ModValue}) ->
          {modifier, [
            {type, "SP"},
            {x, float_to_list(float(Mx))},
            {y, float_to_list(float(My))},
            {value, float_to_list(float(ModValue))}
          ], []}
        end, SPMods),

      ModifiersXML = CDModsXML ++ SPModsXML,

      %%io:format("Player1 Position: ~p,~p~n", [float_to_list(float(P1x)), float_to_list(float(P1y))]),
      %%io:format("Player2 Position: ~p,~p~n", [float_to_list(float(P2x)), float_to_list(float(P2y))]),
      %%io:format("Projectiles1: ~p~n", [Projectiles1]),
      %%io:format("Projectiles2: ~p~n", [Projectiles2]),
      XML_Data = {gamedata, [
        {player1, [
          {x, float_to_list(float(P1x))},
          {y, float_to_list(float(P1y))},
          {score, integer_to_list(Pt1)}
        ], [
          {projectiles, [], Projectiles1}
        ]},
        {player2, [
          {x, float_to_list(float(P2x))},
          {y, float_to_list(float(P2y))},
          {score, integer_to_list(Pt2)}
        ], [
          {projectiles, [], Projectiles2}
        ]},
        {clock, [{time, integer_to_list(Clock)}], []},
        {modifiers, [], ModifiersXML}
      ]},
      XML = lists:flatten(xmerl:export_simple([XML_Data], xmerl_xml)),
      XML;
    {reply, Text} ->
      XML_Data = {reply, [{text, Text}], []},
      XML = lists:flatten(xmerl:export_simple([XML_Data], xmerl_xml)),
      XML;
    {checkLV, LV} ->
      XML_Data = {checkLV, [{level, integer_to_list(LV)}], []},
      XML = lists:flatten(xmerl:export_simple([XML_Data], xmerl_xml)),
      XML;
    {checkStreak, Streak} ->
      XML_Data = {checkStreak, [{streak, integer_to_list(Streak)}], []},
      XML = lists:flatten(xmerl:export_simple([XML_Data], xmerl_xml)),
      XML;
    {leaderboard, Leaderboard} ->
      %% Handle leaderboard data
      LeaderboardXML = lists:map(
        fun({User, {_, Lv, Streak}}) ->
          {user, [
            {username, User},
            {level, integer_to_list(Lv)},
            {streak, integer_to_list(Streak)}
          ], []}
        end, Leaderboard),
      XML_Data = {leaderboard, LeaderboardXML},
      XML = lists:flatten(xmerl:export_simple([XML_Data], xmerl_xml)),
      XML
  end.


send_message(Socket, Message) ->
  Response = format_XMl(Message),
  %%io:format("Sending message: ~p~n", [Response]),
  gen_tcp:send(Socket, io_lib:format("~p~n", [Response])).


