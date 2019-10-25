let inc n = (n+1);;

let hd stream = stream 0;;
let tl stream arg = stream (arg + 1);;

let add stream const arg = const + stream arg;;

let map f stream arg = (f (stream arg));;

let map2 f stream1 stream2 arg = (f (stream1 arg) (stream2 arg));;

let replace stream n a arg =
  if (arg mod n) = 0
  then a
  else stream arg;;

let take stream n arg = stream (n*arg);;

let rec scan stream f a ?(index=0) arg =
  if index = arg
  then f a (stream index)
  else scan stream f (f a (stream index)) ~index:(index + 1) arg ;;

let rec tabulate stream ?(xs = []) ?(left=0) right  =
  if left = right
  then List.rev (List.cons (stream left) xs)
  else tabulate stream ~xs:(List.cons (stream left) xs) ~left:(left + 1) right ;;

let sId n = n;;
let sDouble n = 2*n;;

let inc n = n+1;;
let double n = 2*n;;

hd sId;;
tabulate (tl (tl sId)) 10;;

tabulate (add sDouble 5) 10;;

let sQuadruple = map double sDouble;;

tabulate sQuadruple 10;;

let sTriple = map2 (+) sId sDouble;;

tabulate sTriple 10;;

tabulate (replace sTriple 3 0) 10;;

let sTriple2 = take sId 3;;

tabulate sTriple2 10;;

scan sId (+) 0 5;;
