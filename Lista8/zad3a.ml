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
  module V : VERTEX

  val equal : t -> t -> bool
  val create : vertex -> vertex -> label -> t
  val label : t -> label
  val from : t -> vertex
  val into : t -> vertex
end


module Edge : EDGE with type label = int and module V = Vertex and type vertex = Vertex.t =
struct
  module V = Vertex
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

module Graph (Ver:VERTEX) (Edg:EDGE with type vertex = Ver.t) : GRAPH with module V = Ver and module E = Edg =
struct
  module V = Ver
  type vertex = V.t
  module E = Edg
  type edge = E.t
  type vs = VNil
          | VCons of vertex*vs
  type es = ENil
          | ECons of edge*es
  type t = G of vs*es

  (* funkcje wyszukiwania *)
  let mem_v g v = match g with G(vs, es) ->
    let rec aux vs =
      match vs with
        VNil -> false
      | VCons(hdvs, tlvs) ->
        if V.equal hdvs v
        then true
        else aux tlvs
    in
    aux vs

  let mem_e g e = match g with G(vs, es) ->
    let rec aux es =
      match es with
        ENil -> false
      | ECons(hdes, tles) ->
        if E.equal hdes e
        then true
        else aux tles
    in
    aux es

  let mem_e_v g v1 v2 = match g with G(vs, es) ->
    let rec aux es =
      match es with
        ENil -> false
      | ECons(hdes, tles) ->
        if (V.equal (E.from hdes) v1)&&(V.equal (E.into hdes) v2)
        then true
        else aux tles
    in
    aux es

  let find_e g v1 v2 = match g with G(vs, es) ->
    let rec aux es =
      match es with
        ENil -> failwith "Edge not found"
      | ECons(hdes, tles) ->
        if (V.equal (E.from hdes) v1)&&(V.equal (E.into hdes) v2)
        then hdes
        else aux tles
    in
    aux es

  let succ g v = match g with G(vs, es) ->
    let rec aux succs es =
      match es with
        ENil -> succs
      | ECons(hdes, tles) ->
        if (V.equal (E.from hdes) v)
        then aux ((E.into hdes)::succs) tles
        else aux succs tles
    in
    aux [] es

  let pred g v = match g with G(vs, es) ->
    let rec aux preds es =
      match es with
        ENil -> preds
      | ECons(hdes, tles) ->
        if (V.equal (E.into hdes) v)
        then aux ((E.from hdes)::preds) tles
        else aux preds tles
    in
    aux [] es

  let succ_e g v = match g with G(vs, es) ->
    let rec aux succs es =
      match es with
        ENil -> succs
      | ECons(hdes, tles) ->
        if (V.equal (E.from hdes) v)
        then aux (hdes::succs) tles
        else aux succs tles
    in
    aux [] es

  let pred_e g v = match g with G(vs, es) ->
    let rec aux preds es =
      match es with
        ENil -> preds
      | ECons(hdes, tles) ->
        if (V.equal (E.into hdes) v)
        then aux (hdes::preds) tles
        else aux preds tles
    in
    aux [] es

  (* funkcje modyfikacji *)
  let empty =
    G(VNil, ENil)

  let add_e g e = match g with G(vs, es) ->
    G(vs, ECons(e, es))

  let add_v g v = match g with G(vs, es) ->
    G(VCons(v, vs), es)

  let rem_e g e = match g with G(vs, es) ->
    let rec aux es =
      match es with
        ENil -> ENil
      | ECons(hdes, tles) ->
        if (E.equal hdes e)
        then aux tles
        else ECons(hdes, aux tles)
    in
    G(vs, aux es)

  let rem_v g v = match g with G(vs, es) ->
    let rec aux vs =
      match vs with
        VNil -> VNil
      | VCons(hdvs, tlvs) ->
        if (V.equal hdvs v)
        then aux tlvs
        else VCons(hdvs, aux tlvs)
    in
    G(aux vs, es)

  (* iteratory *)
  let fold_v f g a = match g with G(vs, es) ->
    let rec aux vs =
      match vs with
        VNil -> a
      | VCons(hdvs, tlvs) -> f hdvs (aux tlvs)
    in
    aux vs

  let fold_e f g a = match g with G(vs, es) ->
    let rec aux es =
      match es with
        ENil -> a
      | ECons(hdes, tles) -> f hdes (aux tles)
    in
    aux es
end

module GraphInt = Graph(Vertex)(Edge)

let v1 = Vertex.create 1
let v2 = Vertex.create 2
let v3 = Vertex.create 3
let e1 = Edge.create v1 v2 11
let e2 = Edge.create v1 v3 12
let g1 =
let open GraphInt in
(add_e (add_e (add_v (add_v (add_v empty v1) v2) v3) e1) e2);;

GraphInt.mem_v g1 v1;;
GraphInt.mem_e g1 e1;;
Edge.equal (GraphInt.find_e g1 v1 v2) e1;;
GraphInt.mem_e (GraphInt.rem_e g1 e1) e1;;
List.map Vertex.label (GraphInt.succ g1 v1);;
List.map Vertex.label (GraphInt.pred g1 v2);;
List.map Edge.label (GraphInt.succ_e g1 v1);;


(*
let dfs (type a) (type b) (module G : GRAPH with type t = a and type V.t = b) g v =
  let rec aux visited v =
    List.fold_left aux (v::visited) (G.succ g v)
  in
  List. rev (aux [] v)
   *)

let dfs g v =
  let rec aux visited v =
    let step visited v =
      if not(List.mem v visited)
      then aux visited v
      else visited
    in
    List.fold_left step (v::visited) (GraphInt.succ g v)
  in
  List. rev (aux [] v)

let v4 = Vertex.create 4
let e3 = Edge.create v2 v4 13
let g2 =
  let open GraphInt in
  add_e (add_v g1 v4) e3;;

List.map Vertex.label (dfs g2 v1);;

let bfs g v =
  let rec aux queue visited v =
    match queue with
      [] -> if(List.mem v visited)
      then visited
      else (v::visited)
    | next::qtl -> if(List.mem v visited)
      then aux qtl visited next
      else aux (qtl@(GraphInt.succ g v)) (v::visited) next
  in
  List.rev (aux (GraphInt.succ g v) [] v);;

List.map Vertex.label (bfs g2 v1)
