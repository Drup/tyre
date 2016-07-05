module A = Alcotest
open Tyre

let t' title desc re v s =
  let cre = Tyre.compile re in
  A.(check (result desc reject))
    (title^" exec") (Tyre.exec cre s) (Result.Ok v) ;
  A.(check string) (title^" eval") s (Tyre.eval re v)

let t title desc re v s =
  title, `Quick, fun () -> t' title desc re v s

let topt title desc re v s s' =
  title, `Quick,
  fun () ->
    t' (title ^" some") (A.option desc) re (Some v) s ;
    t' (title ^" none") (A.option desc) re None s'

let basics = [
  t "int" A.int int 42 "42" ;
  t "int pos" A.int pos_int 549085 "549085" ;
  t "int neg" A.int int (-54) "-54" ;

  t "float int" A.float float 3. "3." ;
  (* t "float_int2" A.float float 3. "3" ; *)
  t "float" A.float float 4.2 "4.2" ;
  t "float neg" A.float float (-4.2) "-4.2" ;

  t "bool true" A.bool bool true "true" ;
  t "bool false" A.bool bool false "false" ;

  topt "int option" A.int (opt int) 3 "3" "" ;
  t "int seq" A.(pair int bool) (int <*> bool) (3,true) "3true" ;
]

let prefix_suffix = [
  t "prefix" A.int ((bool,true) **> int) 3 "true3" ;
  t "prefix" A.int ((int,4) **> (bool,false) **> int) (-2) "4false-2" ;

  t "prefixstr" A.int ("foo" *> int) 3 "foo3" ;
  t "suffixstr" A.int (int <* "foo") 3 "3foo" ;

  t "prefix seq" A.(pair int int) (int <*> "foo" *> int) (3,4) "3foo4" ;
  t "prefix seq" A.(pair int bool) (int <*> bool <* "foo") (3,true) "3truefoo" ;
  t "suffix seq" A.(pair int int) (int <* "foo" <*> int) (3,4) "3foo4" ;
  t "suffix seq" A.(pair bool int) ("foo" *> bool <*> int) (true,4) "footrue4" ;
]

let composed = [
  topt "option prefix" A.int (opt int <* "foo") 3 "3foo" "foo" ;
]

let routes =
  let fixed n = regex Re.(repn any n (Some n)) in
  let f n x = n, x in
  route [
    ("foo" *> fixed 3 <* "xx") --> f 1 ;
    ("foo" *> fixed 5) --> f 2 ;
    ("bar" *> fixed 5) --> f 3 ;
    (fixed 2 <* "blob") --> f 4 ;
  ]

let troute title s n res =
  title, `Quick,
  fun () ->
    A.(check @@ result (pair int string) reject)
      title (exec routes s) (Result.Ok (n,res))

let route_test = [
  troute "route 1" "foo123xx" 1 "123" ;
  troute "route 2" "foo12345" 2 "12345" ;
  troute "route 3" "bar12345" 3 "12345" ;
  troute "route 4" "xxblob" 4 "xx" ;
]


let () = Alcotest.run "tyre" [
    "basics", basics ;
    "prefix suffix", prefix_suffix ;
    "composed", composed ;
    "routes", route_test ;
  ]
