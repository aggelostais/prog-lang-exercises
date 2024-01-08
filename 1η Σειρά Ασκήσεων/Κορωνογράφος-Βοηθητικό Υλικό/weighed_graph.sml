(*000000a signature and a structure that implements a weighted graph using an adjacency list*)
(* A signature for directed graphs.*)
signature WGRAPH = sig
  type graph  (* A directed graph comprising a set of vertices
               * V and directed edges E with nonnegative weights. *)
  type vertex (* A vertex, or node, of the graph *)
  type edge   (* A edge of the graph *)

  (* eq(v1,v2) is true iff v1 and v2 are the same vertex. *)
  val eq: vertex * vertex -> bool (*Elegxos an i akmi exei ta idia akra, de mas xreiazetai*)

  (* All vertices in the graph, without duplicates.
   * Run time: O(|V|). *)
  val vertices: graph -> vertex list
 
 (* outgoing(v) is a list of the edges leaving the vertex v.
   * Run time: linear in the length of the result. *) 
  val outgoing: vertex -> edge list

  (* edgeinfo(e) is (src,dst,w) where src is the source of
   * the edge e, dst is its destination, and w is its weight *)
  val edgeinfo: edge -> vertex * vertex * int

  (* add_vertex(g) is (g',v) where g' contains the same vertices 
   * and edges as g, plus a new vertex v with no outgoing edges. *)
  val add_vertex: graph -> graph*vertex

  (* Effects: add_edge(src,dst,w) adds an edge from src to dst,
   * with weight w. *)
  val add_edge: vertex * vertex * int -> unit =
 end


 structure Graph : WGRAPH = struct
  datatype vertex = V of (vertex*int) list ref
  type edge = vertex*vertex*int
  type graph = vertex list

  fun eq(V(v1), V(v2)) = (v1 = v2)
  fun vertices(g) = g
  fun edgeinfo(e) = e
  fun outgoing(V(lr)) = map (fn(dst,w) => (V(lr), dst, w)) (!lr)

  fun add_vertex(g: graph): graph*vertex = 
    let val v = V(ref [])
    in (v::g, v) end

  fun add_edge(src: vertex, dst: vertex, weight: int) =
    case src of V(lr) => lr := (dst,weight)::(!lr)
end