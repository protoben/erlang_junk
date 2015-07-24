-module(euclid).
-export([gcd_sub/2, gcd/2, extended/2, inverse/2]).

%%% Euclidean Algorithm using subtraction
gcd_sub(X, Y) when Y == 0 -> X;
gcd_sub(X, Y) when X == Y -> X;
gcd_sub(X, Y) when X < Y ->
  gcd_sub(X, Y - X);
gcd_sub(X, Y) ->
  gcd_sub(Y, X).

%%% Euclidean Algorithm using modulus
gcd(X, Y) when Y == 0 -> X;
gcd(X, Y) when X == Y -> X;
gcd(X, Y) ->
  gcd(Y, X rem Y).

%%% Extended Euclidean Algorithm
extended(X, Y) when X == Y -> {X, 1, 0};
extended(X, Y) when X > Y ->
  extended({{Y, X}, {0, 1}, {1, 0}});
extended(X, Y) ->
  extended({{X, Y}, {1, 0}, {0, 1}}).

extended({{X, Y}, {S1, _}, {T1, _}}) when Y == 0 ->
  {X, S1, T1};
extended({{X, Y}, {S1, S2}, {T1, T2}}) ->
  Q = X div Y,
  extended({
    {Y, X - (Q * Y)},
    {S2, S1 - (Q * S2)},
    {T2, T1 - (Q * T2)}
  }).

%%% Find the inverse of X in Z[N] using the EEA
inverse(X, N) ->
  {GCD, I, _} = ?MODULE:extended(X, N),
  if
    GCD /= 1 -> exit(not_coprime);
    I < 0    -> N + I;
    true     -> I
  end.
