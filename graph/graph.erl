-module(graph).

-export([new/0,new/1]).
-export([add_vert/2,has_vert/2]).
-export([add_edge/3,add_edge/4]).
-export([add_ve/3,add_ve/4]).

-type graph_type() :: directed | undirected.
-type vertex() :: any().

-record(edge, {source      :: vertex(),
               destination :: vertex(),
               weight      :: number()}).
-record(graph, {type = directed :: graph_type(),
                vertices = []   :: list(vertex()),
                edges = []      :: list(edge())}).
-type edge() :: #edge{}.
-type graph() :: #graph{}.

-export_type([graph_type/0,vertex/0,edge/0,graph/0]).

-spec new()             -> graph().
-spec new(graph_type()) -> graph().
new()     -> #graph{}.
new(T) when T == undirected orelse T == directed ->
  #graph{type = T}.

-spec add_vert(graph(), vertex()) -> graph(). 
add_vert(#graph{vertices = VL} = G, V) ->
  case lists:member(V, VL) of
    true -> G;
    false -> G#graph{vertices = [V | VL]}
  end.

-spec has_vert(graph(), vertex()) -> boolean().
has_vert(#graph{vertices = VL}, V) -> lists:member(V, VL).

-spec add_edge(graph(), vertex(), vertex()) -> graph() | false.
add_edge(G, V1, V2) ->
  G = #graph{vertices = VL, edges = EL},
  case lists:member(V1, VL) andalso lists:member(V1, VL) of
  end,
add_edge(_,_,_,_) -> false.

add_ve(_,_,_) -> false.
add_ve(_,_,_,_) -> false.
