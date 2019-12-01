module type VERTEX = sig
  type t
  type label

  val equal : t -> t -> bool
  val create : label -> t
  val label : t -> label
end

module Vertex : VERTEX with type label = int =
struct
  type label = int
  type t = V of label

  let label v = match v with
      V(l) -> l
  let equal v1 v2 = (label v1) = (label v2)
  let create l = V(l)
end

module type EDGE = sig
  type t
  type label
  type vertex

  val equal : t -> t -> bool
  val create : vertex -> vertex -> label -> t
  val label : t -> label
  val from : t -> vertex
  val into : t -> vertex
end


module Edge (Vertex : VERTEX) : EDGE with type vertex = Vertex.t and type label = int =
struct
  type vertex = Vertex.t
  type label = int
  type t = E of vertex*vertex*label

  let label e = match e with
      E(_,_,label) -> label
  let from e = match e with
      E(v1, _, _) -> v1
  let into e = match e with
      E(_, v2, _) -> v2
  let equal e1 e2 = 
    (Vertex.equal (from e1) (from e2)) && (Vertex.equal (into e1) (into e2)) && (label e1)=(label e2)
  let create v1 v2 l = E(v1,v2,l)
end

module type GRAPH = sig
  (* typ reprezentacji grafu *)
  type t
  module V : VERTEX
  type vertex = V.t
  module E : EDGE with type vertex = vertex
  type edge = E.t

  (* funkcje wyszukiwania *)
  val mem_v : t -> vertex -> bool
  val mem_e : t -> edge -> bool
  val mem_e_v : t -> vertex -> vertex -> bool
  val find_e : t -> vertex -> vertex -> edge
  val succ : t -> vertex -> vertex list
  val pred : t -> vertex -> vertex list
  val succ_e : t -> vertex -> edge list
  val pred_e : t -> vertex -> edge list

  (* funkcje modyfikacji *)
  val empty : t
  val add_e : t -> edge -> t
  val add_v : t -> vertex -> t
  val rem_e : t -> edge -> t
  val rem_v : t -> vertex -> t

  (* iteratory *)
  val fold_v : (vertex ->'a ->'a) -> t ->'a ->'a
  val fold_e : (edge ->'a ->'a) -> t ->'a ->'a
end
