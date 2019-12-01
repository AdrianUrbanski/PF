module type PQUEUE = sig
  type priority
  type 'a t

  exception EmptyPQueue

  val empty : 'a t
  val insert : 'a t -> priority -> 'a -> 'a t
  val remove : 'a t -> priority * 'a * 'a t
end

type comparison = LT | EQ | GT

module type ORDTYPE = sig
  type t

  val compare : t -> t -> comparison
end

module PQueue (OrdType : ORDTYPE) : PQUEUE with type priority = OrdType.t =
struct
  type priority = OrdType.t
  type 'a t = Empty | Push of priority * 'a * 'a t
  exception EmptyPQueue

  let empty = Empty
  let insert pqueue arg_priority arg =
    let rec aux pq =
        match pq with
          Empty -> Push(arg_priority, arg, Empty)
        | Push(elem_priority, elem, pqtl) ->
          if (OrdType.compare arg_priority elem_priority) = GT
          then Push(arg_priority, arg, pq)
          else Push(elem_priority, elem, aux pqtl)
    in
    aux pqueue
  let remove pqueue =
    match pqueue with
      Empty -> failwith("EmptyPQueue")
    | Push(elem_priority, elem, pqtl) ->
      (elem_priority, elem, pqtl)
  end

module OrdInt : ORDTYPE with type t = int =
struct
  type t = int

  let compare a b =
    if a < b
    then LT
    else if a > b
    then GT
    else EQ
end

module PQueueInt = PQueue(OrdInt)

let sort xs =
  let open PQueueInt in
  let rec aux xs pq =
    match xs with
      [] -> ([], pq)
    | (hd::tl) ->
      let (acc, pq) = aux tl (insert pq hd hd) in
      let (elem_priority, elem, pq) = remove pq in
      ((elem::acc), pq)
  in
  let (xs_sorted, _) = aux xs empty
  in
  xs_sorted

let sorted = sort [2; 3; 1; 5; 4]

let sort2 (type a) (module OrdType : ORDTYPE with type t = a) xs =
  let module PQueueType = PQueue(OrdType) in
  let open PQueueType in
  let rec aux xs pq =
    match xs with
      [] -> ([], pq)
    | (hd::tl) ->
      let (acc, pq) = aux tl (insert pq hd hd) in
      let (elem_priority, elem, pq) = remove pq in
      ((elem::acc), pq)
  in
  let (xs_sorted, _) = aux xs empty
  in
  xs_sorted

let sorted2 = sort2 (module OrdInt : ORDTYPE with type t = int) [2; 3; 1; 5; 4]
