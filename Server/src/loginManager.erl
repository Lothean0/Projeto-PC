-module(loginManager).

%% API
-export([create_account/2, close_account/2, login/3, logout/1, check_lv/1,check_streak/1, win/1, lose/1, leaderboard/0, online/0, start/0]).

rpc(Request) ->
  ?MODULE ! {self(), Request},
  receive
    Msg -> Msg
  end.

create_account(User, Pass) -> rpc({create_account, User, Pass}).
close_account(User, Pass) -> rpc({close_account, User, Pass}).
login(User, Pass, Socket) -> rpc({login, User, Pass, Socket}).
logout(User) -> rpc({logout, User}).
check_lv(User) -> rpc({check_level, User}).
check_streak(User) -> rpc({check_streak, User}).
win(User) -> rpc({win, User}).
lose(User) -> rpc({lose, User}).
leaderboard() -> rpc(leaderboard).
online() -> rpc(online).

start() ->
  Map = load_state(),
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
          NewMap = maps:put(U, {P, 1, 0}, Map),
          Pid ! ok,
          save_state(NewMap),
          loop(NewMap, Logged_In)
      end;

    {Pid, {close_account, U}} ->
      case maps:find(U, Map) of
        {ok, _} ->
          NewMap = maps:remove(U, Map),
          Pid ! ok,
          save_state(NewMap),
          loop(NewMap, Logged_In);
        error ->
          Pid ! user_not_found,
          loop(Map, Logged_In)
      end;

    {Pid, {login, U, P, Socket}} ->
      case maps:find(U, Map) of
        {ok, {Pass, _Lv, _Streak}} when Pass == P ->
          case maps:is_key(U, Logged_In) of
            true ->
              Pid ! already_logged_in,
              loop(Map, Logged_In);
            false ->
              NewLogged_In = maps:put(U, Socket, Logged_In),
              Pid ! ok,
              save_state(Map),
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
          save_state(Map),
          loop(Map, NewLogged_In);
        false ->
          Pid ! user_not_logged_in,
          loop(Map, Logged_In)
      end;

    {Pid, {check_level, U}} ->
      case maps:find(U, Map) of
        {ok, {_Pass, Lv, _Streak}} ->
          Pid ! Lv,
          loop(Map, Logged_In);
        error ->
          Pid ! user_not_found,
          loop(Map, Logged_In)
      end;

    {Pid, {check_streak, U}} ->
      case maps:find(U, Map) of
        {ok, {_Pass, _Lv, Streak}} ->
          Pid ! Streak,
          loop(Map, Logged_In);
        error ->
          Pid ! user_not_found,
          loop(Map, Logged_In)
      end;

    {Pid, {win, U}} ->
      case maps:find(U, Map) of
        {ok, {Pass, Lv, Streak}} ->
          NewStreak = Streak + 1,
          NewMap = if
                     NewStreak == Lv ->
                       maps:put(U, {Pass, Lv + 1, 0}, Map);
                     true ->
                       maps:put(U, {Pass, Lv, NewStreak}, Map)
                   end,
          Pid ! ok,
          save_state(NewMap),
          loop(NewMap, Logged_In);
        error ->
          Pid ! user_not_found,
          loop(Map, Logged_In)
      end;

    {Pid, {lose, U}} ->
      case maps:find(U, Map) of
        {ok, {Pass, Lv, Streak}} ->
          NewMap = if
                     Streak >= 0 ->
                       if
                         Lv == 1 ->
                           maps:put(U, {Pass, Lv, 0}, Map);
                         Lv / 2 =< 1 ->
                           NewStreak = 0,
                           maps:put(U, {Pass, Lv - 1, NewStreak}, Map);
                         true ->
                           maps:put(U, {Pass, Lv, Streak - 1}, Map)
                       end;
                     true ->
                       if
                         Lv / 2 =< -(Streak - 1) ->
                           maps:put(U, {Pass, Lv - 1, 0}, Map);
                         true ->
                           NewStreak = Streak - 1,
                           maps:put(U, {Pass, Lv, NewStreak}, Map)
                       end
                   end,
          Pid ! ok,
          save_state(NewMap),
          loop(NewMap, Logged_In);
        error ->
          Pid ! user_not_found,
          loop(Map, Logged_In)
      end;
    {Pid, leaderboard} ->
      Sorted = lists:sort(
        fun({_, {_, Lv1, Streak1}}, {_, {_, Lv2, Streak2}}) ->
          case Lv1 =:= Lv2 of
            true -> Streak1 > Streak2;
            false -> Lv1 > Lv2
          end
        end,
        maps:to_list(Map)
      ),
      Top = lists:sublist(Sorted, 10),
      Pid ! Top,
      loop(Map, Logged_In);


    {Pid, online} ->
      Pid ! Logged_In,
      loop(Map, Logged_In)
  end.

save_state(Map) ->
  file:write_file("accs.dat", term_to_binary(Map)).

load_state() ->
  case file:read_file("accs.dat") of
    {ok, Binary} ->
      Map = binary_to_term(Binary),
      Map;
    _ ->
      #{}
  end.
