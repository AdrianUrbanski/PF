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
  module V: VERTEX
  type vertex = V.t

  val equal : t -> t -> bool
  val create : vertex -> vertex -> label -> t
  val label : t -> label
  val from : t -> vertex
  val into : t -> vertex
end


module Edge : EDGE with type label = int =
struct
  module V = Vertex
  type vertex = V.t
  type label = int
  type t = E of vertex*vertex*label

  let label e = match e with
      E(_,_,label) -> label
  let from e = match e with
      E(v1, _, _) -> v1
  let into e = match e with
      E(_, v2, _) -> v2
  let equal e1 e2 = 
    (V.equal (from e1) (from e2)) && (V.equal (into e1) (into e2)) && (label e1)=(label e2)
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

module type Graph : GRAPH =
struct
  module V = Vertex
  type vertex = V.t
  module E = Edge
  type edge = E.t
  type vs = VNil
          | VCons of vertex*vs
  type es = ENil
          | ECons of edge*es
  type t = G of vs*es

  (* funkcje wyszukiwania *)
  let mem_v (vs, _) v =
    let aux vs =
      match vs with
        VNil -> false
      | VCons(hdvs, tlvs) ->
        if V.equal hdvs v
        then true
        else aux tlvs
    in
    aux vs

  let mem_e (_, es) e =
    let aux es =
      match es with
        ENil -> false
      | ECons(hdes, tles) ->
        if E.equal hdes e
        then true
        else aux tles
    in
    aux vs

  let mem_e_v (_, es) v1 v2 =
    let aux es =
      match es with
        ENil -> false
      | ECons(hdes, tles) ->
        if (V.equal (E.from hdes) v1)&&(V.equal (E.into hdes) v2)
        then true
        else aux tles
    in
    aux vs

  let find_e (_, es) v1 v2 =
    let aux es =
      match es with
        ENil -> failwith "Edge not found"
      | ECons(hdes, tles) ->
        if (V.equal (E.from hdes) v1)&&(V.equal (E.into hdes) v2)
        then hdes
        else aux tles
    in
    aux vs

  let succ (_, es) v =
    let aux succs es =
      match es with
        ENil -> succs
      | ECons(hdes, tles) ->
        if (V.eq (E.from hdes) v)
        then aux ((E.into hdes)::succs) tles
        else aux succs tles
    in
    aux [] es

  let pred (_, es) v =
    let aux preds es =
      match es with
        ENil -> preds
      | ECons(hdes, tles) ->
        if (V.eq (E.into hdes) v)
        then aux ((E.from hdes)::preds) tles
        else aux preds tles
    in
    aux [] es

  let succ_e (_, es) v =
    let aux succs es =
      match es with
        ENil -> succs
      | ECons(hdes, tles) ->
        if (V.eq (E.from hdes) v)
        then aux (hdes::succs) tles
        else aux succs tles
    in
    aux [] es

  let pred_e (_, es) v =
    let aux preds es =
      match es with
        ENil -> preds
      | ECons(hdes, tles) ->
        if (V.eq (E.into hdes) v)
        then aux (hdes::preds) tles
        else aux preds tles
    in
    aux [] es

  (* funkcje modyfikacji *)
  let empty =
    (VNil, ENil)

  let add_e (vs, es) e =
    (vs, ECons(e, es))

  let add_v (vs, es) v =
    (VCons(v, vs), es)

  let rem_e (vs, es) e =
    let aux es =
      match es with
        ENil -> ENil
      | ECons(hdes, tles) ->
        if (E.eq hdes e)
        then aux tles
        else ECons(hdes, aux tles)
    in
    (vs, aux es)

  let rem_v (vs, es) v =
    let aux vs =
      match vs with
        VNil -> VNil
      | VCons(hdvs, tlvs) ->
        if (V.eq hdvs v)
        then aux tlvs
        else VCons(hdvs, aux tlvs)
    in
    (aux vs, es)

  (* iteratory *)
  let fold_v f (vs, es) a =
    let rec aux vs =
      match vs with
        VNil -> a
        VCons(hdvs, tlvs) -> f hdvs (aux tlvs)
    in
    aux vs

  let fold_e f (vs, es) a =
    let rec aux es =
      match es with
        ENil -> a
        ECons(hdes, tles) -> f hdes (aux tles)
    in
    aux es
end
