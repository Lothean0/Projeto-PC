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
      case string:tokens(Data, " ") of
        ["/cr", User, Pass] ->
          %% Create account
          loginManager:create_account(User, Pass),
          gen_tcp:send(Sock, "Account created successfully\n"),
          user_logged_out(Sock);
        ["/cl", User, Pass] ->
          %% Close account
          case loginManager:close_account(User, Pass) of
            ok ->
              gen_tcp:send(Sock, "Account closed successfully\n"),
              user_logged_out(Sock);
            _ ->
              gen_tcp:send(Sock, "Account closure failed\n"),
              user_logged_out(Sock)
          end;
        ["/l", User, Pass] ->
          %% Login
          case loginManager:login(User, Pass, Sock) of
            ok ->
              gen_tcp:send(Sock, "Login successful\n"),
              user_logged_in(Sock, User);
            _ ->
              gen_tcp:send(Sock, "Login failed\n"),
              user_logged_out(Sock)
          end;
        _ ->
          %% Invalid command
          gen_tcp:send(Sock, "Invalid command\n"),
          user_logged_out(Sock)
      end
  end.

user_logged_in(Sock, User) ->
  receive
    {tcp, Sock, Data} ->
      CleanData = string:trim(Data), %% Trim the input
      case string:tokens(CleanData, " ") of
        ["/f"] ->
          %% Send a message to the matchmaker process
          matchmaker ! {self(), {find_match, User, loginManager:check_lv(User)}},
          gen_tcp:send(Sock, "Finding a match...\n"),
          user_logged_in(Sock, User);
        ["/Lv"] ->
          %% Check level
          case loginManager:check_lv(User) of
            user_not_found ->
              gen_tcp:send(Sock, "Failed to check level\n"),
              user_logged_in(Sock, User);
            Lv ->
              gen_tcp:send(Sock, "Your level is: " ++ integer_to_list(Lv) ++ "\n"),
              user_logged_in(Sock, User)
          end;
        ["/q"] ->
          case loginManager:logout(User) of
            ok ->
              gen_tcp:send(Sock, "Logged out successfully\n"),
              user_logged_out(Sock);
            _ ->
              gen_tcp:send(Sock, "Logout failed\n"),
              user_logged_in(Sock, User)
          end;
        _ ->
          gen_tcp:send(Sock, "Invalid command\n"),
          user_logged_in(Sock, User)
      end;
    waiting ->
      gen_tcp:send(Sock, "Searching for a match...\n"),
      user_logged_in(Sock, User);
    {match_found, MatchPid} ->
      gen_tcp:send(Sock, "Match found! MatchPid: " ++ pid_to_list(MatchPid) ++ "\n"),
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
          gen_tcp:send(Sock, "Invalid command\n")
      end,
      match(MatchPid, Sock, User);
    {update, P} ->
      %% Update the position of the player
      gen_tcp:send(Sock, "Your position is: " ++ io_lib:format("~p", [P]) ++ "\n"),
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


