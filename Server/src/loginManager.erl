-module(loginManager).

%% API
-export([create_account/2, close_account/2, login/3, logout/1, online/0, start/0]).

% Client
rpc(Request) ->
  ?MODULE ! {self(), Request},
  receive
    Msg -> Msg
  end.

create_account(User, Pass) -> rpc({create_account, User, Pass}).
close_account(User, Pass) -> rpc({close_account, User, Pass}).
login(User, Pass, Socket) -> rpc({login, User, Pass, Socket}).
logout(User) -> rpc({logout, User}).
online() -> rpc(online).

% Server
start() ->
  Map = #{},
  Logged_In = #{},
  Pid = spawn(fun() -> loop(Map, Logged_In) end),
  register(?MODULE, Pid),
  Pid.

loop(Map, Logged_In) ->
  receive
    {Pid, {create_account, U, P}} ->
      case maps:find(U, Map) of
        {ok, _} ->
          Pid ! user_exists,
          loop(Map, Logged_In);
        error ->
          NewMap = maps:put(U, P, Map),
          Pid ! ok,
          loop(NewMap, Logged_In)
      end;

    {Pid, {close_account, U}} ->
      case maps:find(U, Map) of
        {ok, _} ->
          NewMap = maps:remove(U, Map),
          Pid ! ok,
          loop(NewMap, Logged_In);
        error ->
          Pid ! user_not_found,
          loop(Map, Logged_In)
      end;

    {Pid, {login, U, P, Socket}} ->
      case maps:find(U, Map) of
        {ok, Pass} when Pass == P ->
          case maps:is_key(U, Logged_In) of
            true ->
              Pid ! already_logged_in,
              loop(Map, Logged_In);
            false ->
              NewLogged_In = maps:put(U, Socket, Logged_In),
              Pid ! ok,
              loop(Map, NewLogged_In)
          end;
        {ok, _} ->
          Pid ! wrong_password,
          loop(Map, Logged_In);
        error ->
          Pid ! user_not_found,
          loop(Map, Logged_In)
      end;

    {Pid, {logout, U}} ->
      case maps:is_key(U, Logged_In) of
        true ->
          NewLogged_In = maps:remove(U, Logged_In),
          Pid ! ok,
          loop(Map, NewLogged_In);
        false ->
          Pid ! user_not_logged_in,
          loop(Map, Logged_In)
      end;

    {Pid, online} ->
      Pid ! Logged_In,
      loop(Map, Logged_In)
  end.