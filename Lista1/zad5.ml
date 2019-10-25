let ctrue a b =
  a;;

let cfalse a b =
  b;;

let cand cb1 cb2 a b =
  if cb1 a b = a && cb2 a b = a
  then a
  else b;;

let cor cb1 cb2 a b =
  if cb1 a b = a || cb2 a b = a
  then a
  else b;;

let cbool_of_bool b1 a b =
  if b1
  then a
  else b;;

let bool_of_cbool cb =
  cb true false;;

print_string "cand";;
cand ctrue cfalse true false;;
cand ctrue ctrue true false;;

print_string "cor";;
cor cfalse ctrue true false;;
cor cfalse cfalse true false;;

print_string "cbool_of_bool";;
cbool_of_bool true;;
cbool_of_bool true true false;;
cbool_of_bool false true false;;

print_string "bool_of_cbool";;
bool_of_cbool ctrue;;
bool_of_cbool cfalse;;
