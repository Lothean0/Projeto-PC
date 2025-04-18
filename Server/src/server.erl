-module(server).
-export([start/1, stop/1]).



start(Port) ->
  {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}, {exit_on_close, false},{ip, any}]),
  io:format("Server started on port ~p~n", [Port]),
  loginManager:start(),
  spawn(fun() -> tick_loop() end),
  acceptor(LSock).


stop(Server) ->
  Server ! stop.

acceptor(LSock) ->
  case gen_tcp:accept(LSock) of
    {ok, Sock} ->
      io:format("Client connected: ~p~n", [Sock]),
      spawn(fun() -> acceptor(LSock)  end),
      handle_client(Sock);
    {error, closed} ->
      io:format("Listening socket closed~n")
  end.

handle_client(Sock) ->
  receive
    {tcp, Sock, Data} ->
      io:format("Received data: ~p~n", [Data]),
      case parse_request(Data) of
        {ok, Request} ->
          gen_tcp:send(Sock, process_request(Request,Sock)),
          handle_client(Sock);
        {error, Reason} ->
          gen_tcp:send(Sock, io_lib:format("Error processing request: ~p~n\n",[Reason])),
          handle_client(Sock)
      end
  end.


parse_request(Data) ->
  case string:tokens(string:trim(Data), " ") of
    [Action | Args] ->
      case Action of
        "create_account" -> {ok, {create_account, Args}};
        "close_account" -> {ok, {close_account, Args}};
        "login" -> {ok, {login, Args}};
        "logout" -> {ok, {logout, Args}};
        _ -> {error, invalid_action}
      end;
    _ -> {error, invalid_request}
  end.

process_request(Request,Sock) ->
  case Request of
    {create_account, [User, Pass]} ->
      loginManager:create_account(User, Pass),
      gen_tcp:send(Sock, io_lib:format("Account created successfully for user: ~p~n", [User]));
    {close_account, [User, Pass]} ->
      loginManager:close_account(User, Pass),
      gen_tcp:send(Sock, io_lib:format("Account closed successfully for user: ~p~n", [User]));
    {login, [User, Pass]} ->
      loginManager:login(User, Pass, Sock),
      gen_tcp:send(Sock, io_lib:format("Login successful for user: ~p~n", [User]));
    {logout, [User]} ->
      loginManager:logout(User),
      gen_tcp:send(Sock, io_lib:format("Logout successful for user: ~p~n", [User]));
    _ ->
      gen_tcp:send(Sock, io_lib:format("Invalid request: ~p~n", [Request]))
  end.

tick_loop() ->
  %% Retrieve the list of logged-in users and their sockets
  Logged_In = loginManager:online(),
  %% Send a message to all connected clients
  maps:map(fun(_User, Socket) ->
    gen_tcp:send(Socket, "Server tick: Keep-alive message\n")
           end, Logged_In),
  %% Wait for a second before the next tick
  timer:sleep(1000),
  tick_loop().