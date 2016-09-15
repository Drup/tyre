(** This is a simple example for tyre. *)

(* We create a typed regular expression matching "[0-9]+x[0-9]+"
    <&> is the sequence operator. *> is the prefix operator.
   This typed regular expression will return a pair of positive integers.
*)
let dim : (int * int) Tyre.t = Tyre.( pos_int <&> str"x" *> pos_int )

(* We can keep composing! *)
let prefixed_dim : (int * int) Tyre.t = Tyre.(str"dim:" *> dim)

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
    (fun (x,y) -> { x ; y })
    (fun {x;y} -> (x,y)) (* We provide the backward transformation to unparse *)
    dim

(* We can keep composing regular expressions.
   Here, we parse a list of dimensions ended by semi-colons.

   The <* and *> operators allow to suffix and prefix with a regex.
*)
let list_of_dims : dim list Tyre.t =
  let sep = Tyre.( blanks *> char ';' <* blanks )in
  Tyre.( str"dims:" *> terminated_list ~sep nice_dim )

let () =
  assert (Tyre.eval list_of_dims [{x=2;y=3}; {x=12; y=54}] = "dims:2x3;12x54;")

let list_of_dims_re = Tyre.compile list_of_dims
let () =
  assert (Tyre.exec list_of_dims_re "dims:12x89 ; 60x10 ; 1x1 ;"
      = Result.Ok [{x=12;y=89}; {x=60; y=10}; {x=1;y=1}])
