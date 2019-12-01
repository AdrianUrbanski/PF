module type PQUEUE = sig
  type priority
  type 'a t

  exception EmptyPQueue

  val empty : 'a t
  val insert : 'a t -> priority -> 'a -> 'a t
  val remove : 'a t -> priority * 'a * 'a t
end

module PQueue : PQUEUE with type priority = int =
struct
  type priority = int
  type 'a t = Empty | Push of priority * 'a * 'a t
  exception EmptyPQueue

  let empty = Empty
  let insert pqueue arg_priority arg =
    let rec aux pq =
        match pq with
          Empty -> Push(arg_priority, arg, Empty)
        | Push(elem_priority, elem, pqtl) ->
          if arg_priority > elem_priority
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

let sort xs =
  let open PQueue in
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
