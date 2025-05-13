-module(server).
-export([start/1, stop/1]).



start(Port) ->
  {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}, {exit_on_close, false}, {ip, any}]),
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
      case string:tokens(Data, " ") of
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
        _ ->
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
            Lv ->
              send_message(Sock, {checkLV, Lv}),
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
        _ ->
          send_message(Sock, {reply,"Invalid command"}),
          user_logged_in(Sock, User)
      end;
    already_in_queue ->
      send_message(Sock, {reply,"You are already in the queue for a match."}),
      user_logged_in(Sock, User);
    waiting ->
      send_message(Sock, {reply,"Searching for a match..."}),
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
      CleanData = string:trim(Data), %% Trim the input
      case string:tokens(CleanData, " ") of
        ["/m", Key] ->
          %% Send move command to the match process
          MatchPid ! {move, User, Key, self()};
        _ ->
          %% Handle invalid input
          gen_tcp:send(Sock, "Invalid command")
      end,
      match(MatchPid, Sock, User);
    {update, {P1, P2, Pt1, Pt2, Clock}} ->
      %% Update the position of the player
      send_message(Sock, {gamedata, {P1, P2, Pt1, Pt2, Clock}}),
      match(MatchPid, Sock, User);
    {error, Msg} ->
      %% Handle error messages
      gen_tcp:send(Sock, "Error: " ++ Msg ++ "\n"),
      match(MatchPid, Sock, User);
    win ->
      send_message(Sock, {reply,"You win!"}),
      loginManager:win(User),
      user_logged_in(Sock, User);
    lose ->
      send_message(Sock, {reply,"You lose!"}),
      loginManager:lose(User),
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
    {gamedata, {P1, P2, Pt1, Pt2, Clock}} ->
      {P1x, P1y} = P1,
      {P2x, P2y} = P2,
      XML_Data = {gamedata, [
        {player1, [{x, float_to_list(float(P1x))}, {y, float_to_list(float(P1y))}, {score, integer_to_list(Pt1)}], []},
        {player2, [{x, float_to_list(float(P2x))}, {y, float_to_list(float(P2y))}, {score, integer_to_list(Pt2)}], []},
        {clock, [{time, integer_to_list(Clock)}], []}
      ]},
      XML=lists:flatten(xmerl:export_simple([XML_Data], xmerl_xml)),
      XML;
    {reply, Text} ->
      XML_Data = {reply, [{text, Text}], []},
      XML=lists:flatten(xmerl:export_simple([XML_Data], xmerl_xml)),
      XML;
    {checkLV, LV} ->
      XML_Data = {checkLV, [{level, integer_to_list(LV)}], []},
      XML=lists:flatten(xmerl:export_simple([XML_Data], xmerl_xml)),
      XML
  end.



send_message(Socket, Message) ->
  Response = format_XMl(Message),
  io:format("Sending message: ~p~n", [Response]),
  gen_tcp:send(Socket, io_lib:format("~p~n", [Response])).


