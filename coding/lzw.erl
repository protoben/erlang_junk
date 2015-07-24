-module(lzw).
-export([encode/1, decode/1]).

encode(TEXT) -> encode(TEXT, [], [], 128).
encode(TEXT, _, _, CODEWORD_IDX) when NEXT_CODEWORD == 256 ->
  encode(TEXT);
encode([K | TEXT], W, DICT, LAST, NEXT_CODEWORD) ->
  SYMBOL = W ++ [K]
  case lists:keyfind(SYMBOL, 1, DICT) of
    {_, CODEWORD} ->
      W ++ encode(TEXT, [K], DICT#{SYMBOL => NEXT_CODEWORD}, NEXT_CODEWORD + 1);
    false ->
      encode(TEXT, SYMBOL, DICT, NEXT_CODEWORD)
  end.

decode(STREAM) -> [].
