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
    {tcp, _, Data} ->
      io:format("Received data: ~p~n", [Data]),
      case string:tokens(Data, " ") of
        ["/cr", User, Pass] ->
          case loginManager:create_account(User, Pass) of
            ok ->
              send_message(Sock, {reply, "Account created successfully"}),
              user_logged_out(Sock);
            {error, user_exists} ->
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
    {tcp, Sock, Data} ->
      io:format("Received data: ~p~n", [Data]),
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
    waiting ->
      send_message(Sock, {reply,"Searching for a match..."}),
      user_logged_in(Sock, User);
    {match_found, MatchPid} ->
      send_message(Sock, {reply,"Match found!"}),
      MatchPid ! {connect, User, self()},
      match(MatchPid, Sock, User)
  end.

match(MatchPid, Sock, User) ->
  receive
    {tcp, Sock, Data} ->
      CleanData = string:trim(Data), %% Trim the input
      case string:tokens(CleanData, " ") of
        ["/m", Ax, Ay] ->
          %% Send move command to the match process
          MatchPid ! {move, User, list_to_integer(Ax), list_to_integer(Ay), self()};
        _ ->
          %% Handle invalid input
          gen_tcp:send(Sock, "Invalid command")
      end,
      match(MatchPid, Sock, User);
    {update, {P1,P2}} ->
      {Px1,Py1} = P1,
      {Px2,Py2} = P2,
      %% Update the position of the player
      gen_tcp:send(Sock, io_lib:format("{~p,~p}", [Px1,Py1]) ++ ";" ++ io_lib:format("{~p,~p}", [Px2,Py2]) ++ "\n"),
      match(MatchPid, Sock, User);
    {error, Msg} ->
      %% Handle error messages
      gen_tcp:send(Sock, "Error: " ++ Msg ++ "\n"),
      match(MatchPid, Sock, User);
    win ->
      io:format("Player ~p wins!~n", [User]),
      gen_tcp:send(Sock, "You win!\n"),
      loginManager:win(User),
      user_logged_in(Sock, User);
    lose ->
      io:format("Player ~p loses!~n", [User]),
      gen_tcp:send(Sock, "You lose!\n"),
      loginManager:lose(User),
      user_logged_in(Sock, User)
  end.


format_XMl(Data) ->
  case Data of
    {gamedata, {P1, P2}} ->
      {P1x, P1y} = P1,
      {P2x, P2y} = P2,
      XML_Data = {gamedata,[{player1, [{position, [{x, P1x}, {y, P1y}]}]},
                      {player2, [{position, [{x, P2x}, {y, P2y}]}]}], []},
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


