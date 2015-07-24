-module(ack).
-export([ack/2, ack/3]).

-spec ack(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
ack(0, Y) -> Y + 1;
ack(X, 0) -> ack(X - 1, 1);
ack(X, Y) -> ack(X - 1, ack(X, Y - 1)).

-spec ack(non_neg_integer(), non_neg_integer(), non_neg_integer())
  -> non_neg_integer().
ack(X, Y, 0) -> X + Y;
ack(_X, 0, 1) -> 0;
ack(_X, 0, 2) -> 1;
ack(X, 0, _P) -> X;
ack(X, Y, P) -> ack(X, ack(X, Y - 1, P), P - 1).
