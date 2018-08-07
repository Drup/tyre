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

let map_3 f (x,y,z) = (x, y, f z)

(** {2 The various types} *)

module T = struct

  type ('a, 'b) conv = {
    to_ : 'a -> 'b ;
    from_ : 'b -> 'a ;
  }

  type 'a raw =
    (* We store a compiled regex to efficiently check string when unparsing. *)
    | Regexp : Re.t * Re.re Lazy.t -> string raw
    | Conv   : 'a raw * ('a, 'b) conv -> 'b raw
    | Opt    : 'a raw -> ('a option) raw
    | Alt    : 'a raw * 'b raw -> [`Left of 'a | `Right of 'b] raw
    | Seq    : 'a raw * 'b raw -> ('a * 'b) raw
    | Prefix : 'b raw * 'a raw -> 'a raw
    | Suffix : 'a raw * 'b raw  -> 'a raw
    | Rep    : 'a raw -> 'a Seq.t raw
    | Mod    : (Re.t -> Re.t) * 'a raw -> 'a raw

  type _ wit =
    | Lit    : int -> string wit
    | Conv   : 'a wit * ('a, 'b) conv -> 'b wit
    | Opt    : Re.markid * 'a wit -> 'a option wit
    | Alt    : Re.markid * 'a wit * 'b wit
      -> [`Left of 'a | `Right of 'b] wit
    | Seq    :
        'a wit * 'b wit -> ('a * 'b) wit
    | Rep   : int * 'a wit * Re.re -> 'a Seq.t wit

end

type 'a t = 'a T.raw

let regex x : _ t =
  let re = lazy Re.(compile @@ whole_string @@ no_group x) in
  Regexp (x, re)

let pcre s = regex @@ Re.Pcre.re s

(* Converters

   The exception matching of converters is handled by {!Tyre.exec} directly.
*)
let conv to_ from_ x : _ t =
  Conv (x, {to_; from_})

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
  conv int_of_string string_of_int (regex Regex.pos_int)

let int =
  conv int_of_string string_of_int (regex Regex.int)

let float =
  conv float_of_string string_of_float (regex Regex.float)

let bool =
  conv bool_of_string string_of_bool (regex Regex.bool)

let list e =
  conv Seq.to_list Seq.of_list (rep e)

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
    | Conv (tre, _) -> witnesspp ppf tre
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
let rec pprep f ppf seq = match seq () with
  | Seq.Nil -> ()
  | Cons (x, seq) -> f ppf x ; pprep f ppf seq

let rec evalpp
  : type a . a t -> Format.formatter -> a -> unit
  = fun tre ppf -> let open T in match tre with
    | Regexp (_, lazy cre) -> begin function v ->
        if not @@ Re.execp cre v then
          invalid_arg @@
          Printf.sprintf "Tyre.eval: regexp not respected by \"%s\"." v ;
        pstr ppf v
      end
    | Conv (tre, conv) -> fun v -> evalpp tre ppf (conv.from_ v)
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
  : type a. int -> a t -> int * a T.wit * Re.t
  = let open! Re in let open T in
  fun i -> function
    | Regexp (re, _) ->
      (i+1), Lit i, group @@ no_group re
    | Conv (e, conv) ->
      let i', w, re = build i e in
      i', Conv (w, conv), re
    | Opt e ->
      let i', w, (id, re) = map_3 mark @@ build i e in
      i', Opt (id,w), opt re
    | Alt (e1,e2) ->
      let i', w1, (id1, re1) = map_3 mark @@ build i e1 in
      let i'', w2, re2 = build i' e2 in
      i'', Alt (id1, w1, w2), alt [re1 ; re2]
    | Prefix (e_ign,e) ->
      let i', w, re = build i e in
      let _, _, re_ign = build 1 e_ign in
      i', w, seq [no_group re_ign ; re]
    | Suffix (e,e_ign) ->
      let i', w, re = build i e in
      let _, _, re_ign = build 1 e_ign in
      i', w, seq [re ; no_group re_ign]
    | Seq (e1,e2) ->
      let i', w1, re1 = build i e1 in
      let i'', w2, re2 = build i' e2 in
      i'', Seq (w1, w2), seq [re1; re2]
    | Rep e ->
      let _, w, re = build 1 e in
      (i+1), Rep (i,w,Re.compile re), group @@ rep @@ no_group re
    | Mod (f, e) ->
      let i', w, re = build i e in
      i', w, f re

(** {3 Extraction.} *)

(** Extracting is just a matter of following the witness.
    We just need to take care of counting where we are in the matching groups.

    To avoid copy, we pass around the original string (and we use positions).
*)
let rec extract
  : type a. original:string -> a T.wit -> Re.substrings -> a
  = fun ~original rea s -> let open T in match rea with
    | Lit i -> Re.get s i
    | Conv (w, conv) ->
      let v = extract ~original w s in
      conv.to_ v
    | Opt (id,w) ->
      if not @@ Re.marked s id then None
      else Some (extract ~original w s)
    | Alt (i1,w1,w2) ->
      if Re.marked s i1 then
        `Left (extract ~original w1 s)
      else
        (* Invariant: Alt produces [Re.alt [e1 ; e2]] *)
        `Right (extract ~original w2 s)
    | Seq (e1,e2) ->
      let v1 = extract ~original e1 s in
      let v2 = extract ~original e2 s in
      (v1, v2)
    | Rep (i,e,re) -> extract_list ~original e re i s

(** We need to re-match the string for lists, in order to extract
    all the elements.
    Re doesn't offer the possibility to keep the results when
    grouping under a star (one could argue it's theoretically not
    possible as it would be equivalent to counting in an automaton).
*)
and extract_list
  : type a. original:string -> a T.wit -> Re.re -> int -> Re.Group.t -> a Seq.t
  = fun ~original e re i s ->
    let aux = extract ~original e in
    let (pos, pos') = Re.get_ofs s i in
    let len = pos' - pos in
    Seq.map aux @@ Re.all_seq ~pos ~len re original

(** {4 Multiple match} *)

type +'r route = Route : 'a t * ('a -> 'r) -> 'r route

let route re f = Route (re, f)

let (-->) = route

type 'r wit_route =
    WRoute : Re.markid * 'a T.wit * ('a -> 'r) -> 'r wit_route

(* It's important to keep the order here, since Re will choose
   the first regexp if there is ambiguity.
*)
let rec build_route_aux i rel wl = function
  | [] -> List.rev rel, List.rev wl
  | Route (tre, f) :: l ->
    let i', wit, re = build i tre in
    let id, re = Re.mark re in
    let w = WRoute (id, wit, f) in
    build_route_aux i' (re::rel) (w::wl) l

let build_route l = build_route_aux 1 [] [] l

let rec extract_route ~original wl subs = match wl with
  | [] ->
    (* Invariant: At least one of the regexp of the alternative matches. *)
    assert false
  | WRoute (id, wit, f) :: wl ->
    if Re.Mark.test subs id then
      f (extract ~original wit subs)
    else
      extract_route ~original wl subs

(** {4 Compilation and execution} *)

type 'r info =
  | One of 'r T.wit
  | Routes of 'r wit_route list

type 'a re = { info : 'a info ; cre : Re.re }

let compile tre =
  let _, wit, re = build 1 tre in
  let cre = Re.compile re in
  { info = One wit ; cre }

let route l =
  let rel, wl = build_route l in
  let cre = Re.compile @@ Re.alt rel in
  { info = Routes wl ; cre }


type 'a error = [
  | `NoMatch of 'a re * string
  | `ConverterFailure of exn
]

let extract_with_info ~info ~original subs = match info with
  | One w -> extract ~original w subs
  | Routes wl -> extract_route ~original wl subs

let exec ?pos ?len ({ info ; cre } as tcre) original =
  match Re.exec_opt ?pos ?len cre original with
  | None -> Result.Error (`NoMatch (tcre, original))
  | Some subs ->
    try
      Result.Ok (extract_with_info ~info ~original subs)
    with exn ->
      Result.Error (`ConverterFailure exn)

let execp ?pos ?len {cre ; _ } original =
  Re.execp ?pos ?len cre original

let all_seq ?pos ?len { info ; cre } original =
  let seq = Re.all_seq ?pos ?len cre original in
  let get_res subs = extract_with_info ~info ~original subs in
  Seq.map get_res seq

let all ?pos ?len tcre original =
  try
    Result.Ok (Seq.to_list @@ all_seq ?pos ?len tcre original)
  with exn ->
    Result.Error (`ConverterFailure exn)

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
  | Conv (tre,_) -> sexp ppf "Conv" "%a" pp tre
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
  : type a. _ -> a T.wit -> unit
  = fun ppf -> let open T in function
  | Lit i -> sexp ppf "Lit" "%i" i
  | Conv (tre,_) -> sexp ppf "Conv" "%a" pp_wit tre
  | Opt (_, tre) -> sexp ppf "Opt" "%a" pp_wit tre
  | Alt (_, tre1, tre2) -> sexp ppf "Alt" "%a@ %a" pp_wit tre1 pp_wit tre2
  | Seq (tre1 ,tre2) -> sexp ppf "Seq" "%a@ %a" pp_wit tre1 pp_wit tre2
  | Rep (i, w, re) -> sexp ppf "Rep" "%i@ %a@ %a" i pp_wit w Re.pp_re re

let pp_wit_route
  : type a. _ -> a wit_route -> unit
  = fun ppf (WRoute (_,w,_)) -> pp_wit ppf w

let pp_re ppf = function
  | { info = One w; cre } ->
    sexp ppf "One" "%a@ %a" Re.pp_re cre pp_wit w
  | { info = Routes wl; cre } ->
    sexp ppf "Route" "%a@ %a" Re.pp_re cre (pp_list pp_wit_route) wl

let pp_error ppf : _ error -> unit = function
  | `NoMatch (re, s) ->
    Format.fprintf ppf "`NoMatch (%a, %s)" pp_re re s
  | `ConverterFailure exn ->
    Format.pp_print_string ppf @@ Printexc.to_string exn

module Internal = struct
  include T

  let to_t x = x
  let from_t x = x

  let build = build
  let extract = extract
end
