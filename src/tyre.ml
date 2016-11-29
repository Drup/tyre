(*
 * Copyright (c) 2016 Gabriel Radanne <drupyog@zoho.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Copied from gen.ml *)
module Gen = struct

  let map f gen =
    let stop = ref false in
    fun () ->
      if !stop then None
      else match gen() with
        | None -> stop:= true; None
        | Some x -> Some (f x)

  (* Copied from gen.ml *)
  let of_list l =
    let l = ref l in
    fun () ->
      match !l with
      | [] -> None
      | x::l' -> l := l'; Some x
  let rec fold f acc gen =
    match gen () with
    | None -> acc
    | Some x -> fold f (f acc x) gen
  let to_rev_list gen =
    fold (fun acc x -> x :: acc) [] gen
  let to_list gen = List.rev (to_rev_list gen)

end

let map_snd f (x,y) = (x, f y)
let map_3 f (x,y,z) = (x, y, f z)

(** {2 The various types} *)

type 'a gen = unit -> 'a option

module T = struct

  type ('a, 'b) conv = {
    to_ : 'a -> 'b option ;
    from_ : 'b -> 'a ;
  }

  type 'a raw =
    (* We store a compiled regex to efficiently check string when unparsing. *)
    | Regexp : Re.t * Re.re Lazy.t -> string raw
    | Conv   : string * 'a raw * ('a, 'b) conv -> 'b raw
    | Opt    : 'a raw -> ('a option) raw
    | Alt    : 'a raw * 'b raw -> [`Left of 'a | `Right of 'b] raw
    | Seq    : 'a raw * 'b raw -> ('a * 'b) raw
    | Prefix : 'b raw * 'a raw -> 'a raw
    | Suffix : 'a raw * 'b raw  -> 'a raw
    | Rep    : 'a raw -> 'a gen raw
    | Mod    : (Re.t -> Re.t) * 'a raw -> 'a raw

  type _ wit =
    | Regexp : Re.t -> string wit
    | Conv   : string * 'a wit * ('a, 'b) conv -> 'b wit
    | Opt    : Re.markid * int * 'a wit -> 'a option wit
    | Alt    : Re.markid * int * 'a wit * Re.markid * 'b wit
      -> [`Left of 'a | `Right of 'b] wit
    | Seq    :
        'a wit * 'b wit -> ('a * 'b) wit
    | Rep   : 'a wit * Re.re -> 'a gen wit

end

type 'a t = 'a T.raw

let regex x : _ t =
  let re = lazy Re.(compile @@ whole_string @@ no_group x) in
  Regexp (x, re)

let conv_fail ~name to_ from_ x : _ t =
  let to_ x = try to_ x with _ -> None in
  Conv (name, x, {to_; from_})

let conv to_ from_ x : _ t =
  let to_ x = Some (to_ x) in
  Conv ("", x, {to_; from_})

let seq a b : _ t = Seq (a, b)
let alt a b : _ t = Alt (a, b)

let prefix x a : _ t = Prefix (x, a)
let suffix a x : _ t = Suffix (a, x)
let opt a : _ t = Opt a

module Infix = struct

  let (<|>) = alt
  let (<&>) = seq

  let ( *>) = prefix
  let (<* ) = suffix

end
include Infix

let rep x : _ t = Rep x
let rep1 x = x <&> rep x

(* [modifier] is unsafe in general (for example [modifier Re.group]).
   It shouldn't be exposed to the user.
*)
let modifier f re : _ t = Mod (f, re)

let word re = modifier Re.word re
let whole_string re = modifier Re.whole_string re
let longest re = modifier Re.longest re
let shortest re = modifier Re.shortest re
let first re = modifier Re.first re
let greedy re = modifier Re.greedy re
let non_greedy re = modifier Re.non_greedy re
let nest re = modifier Re.nest re

module Regex = struct
  open! Re

  (** [0-9]+ *)
  let pos_int = rep1 digit

  (** -?[0-9]+ *)
  let int =
    seq [opt (char '-') ; pos_int]

  (** -?[0-9]+( .[0-9]* )? *)
  let float =
    seq [opt (char '-') ; rep1 digit ; opt (seq [char '.'; rep digit])]

  (** true|false *)
  let bool =
    alt [str "true" ; str "false"]

end

let some f x = Some (f x)

let unit s re =
  conv
    (fun _ -> ())
    (fun () -> s)
    (regex re)

let start = unit "" Re.start
let stop = unit "" Re.stop

let str s = unit s (Re.str s)

let char c =
  let s = String.make 1 c in
  unit s (Re.char c)

let blanks = unit "" (Re.rep Re.blank)

let pos_int =
  conv_fail "pos_int" (some int_of_string) string_of_int (regex Regex.pos_int)

let int =
  conv_fail "int" (some int_of_string) string_of_int (regex Regex.int)

let float =
  conv_fail "float" (some float_of_string) string_of_float (regex Regex.float)

let bool =
  conv_fail "bool" (some bool_of_string) string_of_bool (regex Regex.bool)

let list e =
  conv Gen.to_list Gen.of_list (rep e)

let terminated_list ~sep e = list (e <* sep)
let separated_list ~sep e =
  let e = opt (e <&> list (sep *> e)) in
  let to_ = function None -> [] | Some (h, t) -> (h :: t)
  and from_ = function [] -> None | h :: t -> Some (h, t)
  in
  conv to_ from_ e


(** {2 Witness} *)

(** A witness is a string such that [exec (compile re) (witness re) = true].
    The computation of the witness is deterministic and should result in
    a small example.

    It is used in [eval] for the part of the regex that are ignored.
*)

let rec witnesspp
  : type a . Format.formatter -> a t -> unit
  = fun ppf tre -> let open T in match tre with
    | Regexp (re, _) -> Format.pp_print_string ppf @@ Re.witness re
    | Conv (_, tre, _) -> witnesspp ppf tre
    | Opt _ -> ()
    | Alt (tre1, _) -> witnesspp ppf tre1
    | Seq (tre1, tre2) ->
      witnesspp ppf tre1 ;
      witnesspp ppf tre2
    | Prefix (tre1,tre2) ->
      witnesspp ppf tre1 ;
      witnesspp ppf tre2
    | Suffix (tre1,tre2) ->
      witnesspp ppf tre1 ;
      witnesspp ppf tre2
    | Rep _ -> ()
    | Mod (_,tre) ->
      witnesspp ppf tre

(** {2 Evaluation functions} *)

(** Evaluation is the act of filling the holes. *)

let pstr = Format.pp_print_string
let rec pprep f ppf gen = match gen () with
  | None -> ()
  | Some x -> f ppf x ; pprep f ppf gen

let rec evalpp
  : type a . a t -> Format.formatter -> a -> unit
  = fun tre ppf -> let open T in match tre with
    | Regexp (_, lazy cre) -> begin function v ->
        if not @@ Re.execp cre v then
          invalid_arg @@
          Printf.sprintf "Tyre.eval: regexp not respected by \"%s\"." v ;
        pstr ppf v
      end
    | Conv (_, tre, conv) -> fun v -> evalpp tre ppf (conv.from_ v)
    | Opt p -> begin function
        | None -> pstr ppf ""
        | Some x -> evalpp p ppf x
      end
    | Seq (tre1,tre2) -> fun (x1, x2) ->
      evalpp tre1 ppf x1 ;
      evalpp tre2 ppf x2 ;
    | Prefix(tre_l,tre) ->
      fun v -> witnesspp ppf tre_l ; evalpp tre ppf v
    | Suffix(tre,tre_g) ->
      fun v -> evalpp tre ppf v ; witnesspp ppf tre_g
    | Alt (treL, treR) -> begin function
        | `Left x -> evalpp treL ppf x
        | `Right x -> evalpp treR ppf x
      end
    | Rep tre ->
      pprep (evalpp tre) ppf
    | Mod (_, tre) -> evalpp tre ppf

let eval tre = Format.asprintf "%a" (evalpp tre)

(** {2 matching} *)

(** {3 Regexp construction}

    In order to record how we constructed the regexp and how to later
    extract information, we build a witness containing all the tools we need.

    Each alternative is marked with {!Re.mark}. We store the markid in order
    to be able to guess the branch matched.
*)

let rec build
  : type a. a t -> int * a T.wit * Re.t
  = let open! Re in let open T in function
    | Regexp (re, _) ->
      1, Regexp re, group @@ no_group re
    | Conv (name , e, conv) ->
      let i, w, re = build e in
      i, Conv (name, w, conv), re
    | Opt e ->
      let i, w, (id, re) = map_3 mark @@ build e in
      i, Opt (id,i,w), opt re
    | Alt (e1,e2) ->
      let i1, w1, (id1, re1) = map_3 mark @@ build e1 in
      let i2, w2, (id2, re2) = map_3 mark @@ build e2 in
      let grps = i1 + i2 in
      grps, Alt (id1, i1, w1, id2, w2), alt [re1 ; re2]
    | Prefix (e_ign,e) ->
      let i, w, re = build e in
      let _, _, re_ign = build e_ign in
      i, w, seq [no_group re_ign ; re]
    | Suffix (e,e_ign) ->
      let i, w, re = build e in
      let _, _, re_ign = build e_ign in
      i, w, seq [re ; no_group re_ign]
    | Seq (e1,e2) ->
      let i1, w1, re1 = build e1 in
      let i2, w2, re2 = build e2 in
      let grps = i1 + i2 in
      grps, Seq (w1, w2), seq [re1; re2]
    | Rep e ->
      let _, w, re = build e in
      1, Rep (w,Re.compile re), group @@ rep @@ no_group re
    | Mod (f, e) ->
      let n, w, re = build e in
      n, w, f re

(** {3 Extraction.} *)

exception ConverterFailure of string * string

(** Extracting is just a matter of following the witness.
    We just need to take care of counting where we are in the matching groups.

    To avoid copy, we pass around the original string (and we use positions).
*)
let rec extract
    | Regexp _ -> i+1, Re.get s i
  : type a. original:string -> a T.wit -> int -> Re.substrings -> int * a
  = fun ~original rea i s -> let open T in match rea with
    | Conv (name, w, conv) ->
      let i, v = extract ~original w i s in
      begin match conv.to_ v with
        | Some v -> (i, v)
        | None -> raise (ConverterFailure (name, Re.get s i))
      end
    | Opt (id,grps,w) ->
      if not @@ Re.marked s id then i+grps, None
      else map_snd (fun x -> Some x) @@ extract ~original w i s
    | Alt (i1,grps,w1,id2,w2) ->
      if Re.marked s i1 then
        map_snd (fun x -> `Left x) @@ extract ~original w1 i s
      else if Re.marked s id2 then
        map_snd (fun x -> `Right x) @@ extract ~original w2 (i+grps) s
      else
        (* Invariant: Alt produces [Re.alt [e1 ; e2]] *)
        assert false
    | Seq (e1,e2) ->
      let i, v1 = extract ~original e1 i s in
      let i, v2 = extract ~original e2 i s in
      i, (v1, v2)
    | Rep (e,re) -> i+1, extract_list ~original e re i s

and extract_top
  : type a . original:string -> a T.wit -> Re.substrings -> a
  = fun ~original e s -> snd @@ extract ~original e 1 s

(** We need to re-match the string for lists, in order to extract
    all the elements.
    Re doesn't offer the possibility to keep the results when
    grouping under a star (one could argue it's theoretically not
    possible as it would be equivalent to counting in an automaton).
*)
and extract_list
  : type a. original:string -> a T.wit -> Re.re -> int -> Re.substrings -> a gen
  = fun ~original e re i s ->
    let aux = extract_top ~original e in
    let (pos, pos') = Re.get_ofs s i in
    let len = pos' - pos in
    Gen.map aux @@ Re.all_gen ~pos ~len re original

(** {4 Multiple match} *)

type +'r route = Route : 'a t * ('a -> 'r) -> 'r route

let route re f = Route (re, f)

let (-->) = route

type 'r wit_route =
    WRoute : Re.markid * int * 'a T.wit * ('a -> 'r) -> 'r wit_route

(* It's important to keep the order here, since Re will choose
   the first regexp if there is ambiguity.
*)
let rec build_route_aux rel wl = function
  | [] -> List.rev rel, List.rev wl
  | Route (tre, f) :: l ->
    let grps, wit, re = build tre in
    let id, re = Re.mark re in
    let w = WRoute (id, grps, wit, f) in
    build_route_aux (re::rel) (w::wl) l

let build_route l = build_route_aux [] [] l

let rec extract_route ~original i subs = function
  | [] ->
    (* Invariant: At least one of the regexp of the alternative matches. *)
    assert false
  | WRoute (id, grps, wit, f) :: l ->
    if Re.Mark.test subs id then
      let _, v = extract ~original wit i subs in f v
    else
      extract_route ~original (i+grps) subs l

let extract_route_top ~original l subs = extract_route ~original 1 subs l

(** {4 Compilation and execution} *)

type 'r info =
  | One of 'r T.wit
  | Routes of 'r wit_route list

type 'a re = { info : 'a info ; cre : Re.re }

let compile tre =
  let _, wit, re = build tre in
  let cre = Re.compile re in
  { info = One wit ; cre }

let route l =
  let rel, wl = build_route l in
  let cre = Re.compile @@ Re.alt rel in
  { info = Routes wl ; cre }


type 'a error = [
  | `NoMatch of 'a re * string
  | `ConverterFailure of string * string
]

let exec ?pos ?len ({ info ; cre } as tcre) original =
  match Re.exec_opt ?pos ?len cre original with
  | None -> Result.Error (`NoMatch (tcre, original))
  | Some subs ->
    let f = match info with
      | One wit -> extract_top ~original wit
      | Routes wl -> extract_route_top ~original wl
    in
    try
      Result.Ok (f subs)
    with ConverterFailure (name, s) ->
      Result.Error (`ConverterFailure (name, s))

let execp ?pos ?len {cre ; _ } original =
  Re.execp ?pos ?len cre original

(** Pretty printers *)

let sexp ppf s fmt = Format.fprintf ppf ("@[<3>(%s@ "^^fmt^^")@]") s

(* Only in the stdlib since 4.02, so we copy. *)
let rec pp_list pp ppf = function
  | [] -> ()
  | [v] -> pp ppf v
  | v :: vs ->
    pp ppf v;
    Format.pp_print_space ppf ();
    pp_list pp ppf vs

let rec pp
  : type a. _ -> a t -> unit
  = fun ppf -> let open T in function
  | Regexp (re,_) -> sexp ppf "Re" "%a" Re.pp re
  | Conv (name,tre,_) -> sexp ppf "Conv" "%s@ %a)" name pp tre
  | Opt tre -> sexp ppf "Opt" "%a" pp tre
  | Alt (tre1, tre2) -> sexp ppf "Alt" "%a@ %a" pp tre1 pp tre2
  | Seq (tre1 ,tre2) -> sexp ppf "Seq" "%a@ %a" pp tre1 pp tre2
  | Prefix (tre1, tre2) ->
    sexp ppf "Prefix" "%a@ %a" pp tre1 pp tre2
  | Suffix (tre1, tre2) ->
    sexp ppf "Suffix" "%a@ %a" pp tre1 pp tre2
  | Rep tre -> sexp ppf "Rep" "%a" pp tre
  | Mod (_,tre) -> sexp ppf "Mod" "%a" pp tre

let rec pp_wit
  | Regexp re -> sexp ppf "Re" "%a" Re.pp re
  : type a. _ -> a T.wit -> unit
  = fun ppf -> let open T in function
  | Conv (name, tre,_) -> sexp ppf "Conv" "%s@ %a)" name pp_wit tre
  | Opt (_, _, tre) -> sexp ppf "Opt" "%a" pp_wit tre
  | Alt (_, _, tre1, _, tre2) -> sexp ppf "Alt" "%a@ %a" pp_wit tre1 pp_wit tre2
  | Seq (tre1 ,tre2) -> sexp ppf "Seq" "%a@ %a" pp_wit tre1 pp_wit tre2
  | Rep (w, re) -> sexp ppf "Rep" "%a@ %a" pp_wit w Re.pp_re re

let pp_wit_route
  : type a. _ -> a wit_route -> unit
  = fun ppf (WRoute (_,_,w,_)) -> pp_wit ppf w

let pp_re ppf = function
  | { info = One w; cre } ->
    sexp ppf "One" "%a@ %a" Re.pp_re cre pp_wit w
  | { info = Routes wl; cre } ->
    sexp ppf "Route" "%a@ %a" Re.pp_re cre (pp_list pp_wit_route) wl

module Internal = struct
  include T

  let to_t x = x
  let from_t x = x

  exception ConverterFailure = ConverterFailure

  let build = build
  let extract = extract
end
