(** This is a simple example for tyre. *)

(* We create a typed regular expression matching "[0-9]+x[0-9]+"
    <*> is the sequence operator. *> is the prefix operator.
   This typed regular expression will return a pair of positive integers.
*)
let dim : (int * int) Tyre.t = Tyre.( pos_int <*> "x" *> pos_int )

(* We can keep composing! *)
let prefixed_dim : (int * int) Tyre.t = Tyre.("dim:" *> dim)

(* Before using it, we need to compile it *)
let dim_re = Tyre.compile prefixed_dim

(* We can now use it to parse. *)
let () =
  assert (Tyre.exec dim_re "dim:23x10" = Result.Ok (23, 10))

(* We can also use it to unparse! *)
let () =
  assert (Tyre.eval prefixed_dim (5, 2) = "dim:5x2")
(* Note that unparsing doesn't need the compiled regular expression.
   Unparsing can never fail.
*)

(* Pairs are fine, but we want pretty types.
   We can use converters to transform the value. *)
type dim = { x : int ; y : int }
let nice_dim : dim Tyre.t =
  Tyre.conv
    ~name:"dim" (* Name of the converter, for error reporting. *)
    (fun (x,y) -> Some { x ; y }) (* forward, can potentially fail with None *)
    (fun {x;y} -> (x,y)) (* We provide the backward transformation to unparse *)
    dim

(* We can keep composing regular expressions and fall back to Re when needed.
   Here, we parse a list of dimensions ended by semi-colons.

   The <** operator allow to suffix with a regex.
   Since tyre is bidirectional, we need to provide something that should be
   used for unparsing. Here it's simply ",".
*)
let list_of_dims : dim list Tyre.t =
  let sep = Re.(seq [ rep blank ; char ';' ; rep blank ]) in
  Tyre.( "dims:" *> list (nice_dim <** (regex sep, ";")))

let () =
  assert (Tyre.eval list_of_dims [{x=2;y=3}; {x=12; y=54}] = "dims:2x3;12x54;")

let list_of_dims_re = Tyre.compile list_of_dims
let () =
  assert (Tyre.exec list_of_dims_re "dims:12x89 ; 60x10 ; 1x1 ;"
      = Result.Ok [{x=12;y=89}; {x=60; y=10}; {x=1;y=1}])
