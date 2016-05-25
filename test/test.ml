module A = struct
  include Alcotest
  let float = of_pp Format.pp_print_float
end
open Tyre

let t' title desc re v s =
  let cre = Tyre.compile re in
  A.(check (option desc)) (title^" exec") (Tyre.exec cre s) (Some v) ;
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
]

let prefix_suffix = [
  t "prefix" A.int ("foo" *> int) 3 "foo3" ;
  t "suffix" A.int (int <* "foo") 3 "3foo" ;
]

let composed = [
  topt "option" A.int (opt int) 3 "3" "" ;
  topt "option prefix" A.int (opt int <* "foo") 3 "3foo" "foo" ;
]

let () = Alcotest.run "tyre" [
    "basics", basics ;
    "prefix suffix", prefix_suffix ;
    "composed", composed ;
  ]
