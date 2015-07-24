-module(primes).
-export([esieve/1, ftest/1]).

esieve(2) -> [2];
esieve(Max) when is_integer(Max)
           andalso Max > 1 ->
  List = seq(3, Max, 2),
  [2 | do_esieve(List)].

do_esieve([Factor | List]) ->
  Coprimes = lists:filter(fun(X) -> X rem Factor /= 0 end, List),
  [Factor | do_esieve(Coprimes)];
do_esieve([]) ->
  [].

ftest(X) when X > 1 ->
  random:seed(now()),
  Rand = random:uniform(X - 2) + 1,
  ftest(X, 20, Rand).

ftest(_, Reps, _) when Reps == 0 -> true;
ftest(X, Reps, Witness) ->
  Test = pow(Witness, X - 1) rem X,
  case Test of
    1 ->
      Rand = random:uniform(X - 2) + 1,
      ftest(X, Reps - 1, Rand);
    _ ->
      false
  end.

seq(Min, Max, Inc) when Min < Max ->
  [Min | seq(Min + Inc, Max, Inc)];
seq(Min, Max, _) when Min >= Max ->
  [Max].

pow(_, Y) when Y == 0 -> 1;
pow(X, Y) when Y > 0 ->
  X * pow(X, Y - 1).
