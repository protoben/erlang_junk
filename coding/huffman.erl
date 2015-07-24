-module(huffman).
-export([encode/1, decode/1, codebook/1]).
-export([symbol_freq/1, codetree/1]).

encode(TEXT) when is_list(TEXT) or is_binary(TEXT) ->
  CODEBOOK = codebook(TEXT),
  {CODEBOOK, encode(TEXT, CODEBOOK)}.
encode([], _) -> <<>>;
encode([HEAD | REST], CODEBOOK) ->
  {HEAD, SYMBOL} = lists:keyfind(HEAD, 1, CODEBOOK),
  STREAM = encode(REST, CODEBOOK),
  <<SYMBOL/bits, STREAM/bits>>.

decode({CODEBOOK, STREAM}) -> decode(CODEBOOK, STREAM, <<>>).
decode(_, <<>>, _) -> [];
decode(CODEBOOK, <<BIT:1, STREAM/bits>>, <<MOREBITS/bits>>) ->
  CODEWORD = <<MOREBITS/bits, BIT:1>>,
  WIDTH = erlang:bit_size(CODEWORD),
  case lists:keyfind(CODEWORD, 2, CODEBOOK) of
    {SYMBOL, <<_:WIDTH/bits>>} -> [SYMBOL | decode(CODEBOOK, STREAM, <<>>)];
    _ -> decode(CODEBOOK, STREAM, CODEWORD)
  end.

symbol_freq(TEXT) when is_list(TEXT) -> symbol_freq(TEXT, []);
symbol_freq(TEXT) when is_binary(TEXT) -> symbol_freq(bin:to_list(TEXT), []).
symbol_freq([], FLIST) -> FLIST;
symbol_freq([HEAD | REST], FLIST) ->
  case lists:keyfind(HEAD, 1, FLIST) of
    false ->
      symbol_freq(REST, [{HEAD, 1} | FLIST]);
    {HEAD, FREQ} ->
      symbol_freq(REST, lists:keyreplace(HEAD, 1, FLIST, {HEAD, FREQ + 1}))
  end.

codetree(TEXT) when is_list(TEXT) or is_binary(TEXT) ->
  FLIST = symbol_freq(TEXT),
  make_codetree(lists:keysort(2, FLIST)).
make_codetree([{TREE, _}]) -> TREE;
make_codetree([{KEY1, FREQ1}, {KEY2, FREQ2} | REST]) ->
  FLIST = [{{KEY1, KEY2}, FREQ1 + FREQ2} | REST],
  make_codetree(lists:keysort(2, FLIST)).

codebook(TEXT) when is_list(TEXT) or is_binary(TEXT) ->
  TREE = codetree(TEXT),
  tree_to_book(TREE, <<>>).
tree_to_book({ZERO, ONE}, <<CODEWORD/bits>>) ->
  tree_to_book(ZERO, <<CODEWORD/bits, 0:1>>) ++ tree_to_book(ONE, <<CODEWORD/bits, 1:1>>);
tree_to_book(SYMBOL, CODEWORD) when is_integer(SYMBOL) ->
  [{SYMBOL, CODEWORD}].
