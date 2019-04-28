
type ('a, 'b) conv = {
  to_ : 'a -> 'b ;
  from_ : 'b -> 'a ;
}

module Seq = struct
  include Seq

  let of_list l =
    let rec aux l () = match l with
      | [] -> Seq.Nil
      | x :: tail -> Seq.Cons (x, aux tail)
    in
    aux l
  let to_rev_list gen =
    fold_left (fun acc x -> x :: acc) [] gen
  let to_list gen = List.rev (to_rev_list gen)
end
