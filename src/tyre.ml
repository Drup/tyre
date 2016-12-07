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

(** {2 The various types} *)

type 'a gen = unit -> 'a option

module T = struct

  type ('a, 'b) conv = {
    to_ : ('a -> 'b) expr ;
    from_ : ('b -> 'a) expr ;
  }

  type 'a raw =
    (* We store a compiled regex to efficiently check string when unparsing. *)
    | Regexp : Re.t expr * Re.re Lazy.t expr -> string raw
    | Conv   : 'a raw * ('a, 'b) conv -> 'b raw
    | Opt    : 'a raw -> ('a option) raw
    | Alt    : 'a raw * 'b raw -> [`Left of 'a | `Right of 'b] raw
    | Seq    : 'a raw * 'b raw -> ('a * 'b) raw
    | Prefix : 'b raw * 'a raw -> 'a raw
    | Suffix : 'a raw * 'b raw  -> 'a raw
    | Rep    : 'a raw -> 'a gen raw
    | Mod    : (Re.t -> Re.t) expr * 'a raw -> 'a raw

  type _ wit =
    | Lit    : int -> string wit
    | Conv   : 'a wit * ('a, 'b) conv -> 'b wit
    | Opt    : Re.markid expr * 'a wit -> 'a option wit
    | Alt    : Re.markid expr * 'a wit * 'b wit
      -> [`Left of 'a | `Right of 'b] wit
    | Seq    :
        'a wit * 'b wit -> ('a * 'b) wit
    | Rep   : int * 'a wit * Re.re expr -> 'a gen wit

end

type 'a t = 'a T.raw

macro regex x : _ t =
  let re = << lazy Re.(compile (whole_string (no_group $x))) >> in
  Regexp (x, re)

(* Converters

   The implementation here assume converter failures are rare.
   This way, we avoid allocating an option and using an exception handler in
   the non-failing case.
*)
exception ConverterFailure of exn

let fail exn =
  raise (ConverterFailure exn)

macro conv_fail to_ from_ x : _ t =
  let to_ =
    << fun x -> match $to_ x with
      | Result.Ok x -> x
      | Result.Error exn -> fail exn
      | exception exn -> fail exn
    >>
  in
  Conv (x, {to_; from_})

static conv to_ from_ x : _ t =
  Conv (x, {to_; from_})

static seq a b : _ t = Seq (a, b)
static alt a b : _ t = Alt (a, b)

static prefix x a : _ t = Prefix (x, a)
static suffix a x : _ t = Suffix (a, x)
static opt a : _ t = Opt a

module Infix = struct

  static (<|>) = alt
  static (<&>) = seq

  static ( *>) = prefix
  static (<* ) = suffix

end
include Infix

static rep x : _ t = Rep x
static rep1 x = x <&> rep x

(* [modifier] is unsafe in general (for example [modifier Re.group]).
   It shouldn't be exposed to the user.
*)
static modifier f re : _ t = Mod (f, re)

macro word re = modifier <<Re.word>> re
macro whole_string re = modifier <<Re.whole_string>> re
macro longest re = modifier <<Re.longest>> re
macro shortest re = modifier <<Re.shortest>> re
macro first re = modifier <<Re.first>> re
macro greedy re = modifier <<Re.greedy>> re
macro non_greedy re = modifier <<Re.non_greedy>> re
macro nest re = modifier <<Re.nest>> re

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

macro unit s re =
  conv
    <<(fun _ -> ())>>
    <<(fun () -> $s)>>
    (regex re)

macro start = unit <<"">> <<Re.start>>
macro stop = unit <<"">> <<Re.stop>>

macro str s = unit s <<Re.str $s>>

macro char c =
  let s = << String.make 1 $c >> in
  unit s <<Re.char $c>>

macro blanks = unit <<"">> <<Re.rep Re.blank>>

macro pos_int =
  conv <<int_of_string>> <<string_of_int>> (regex <<Regex.pos_int>>)

macro int =
  conv <<int_of_string>> <<string_of_int>> (regex <<Regex.int>>)

macro float =
  conv <<float_of_string>> <<string_of_float>> (regex <<Regex.float>>)

macro bool =
  conv <<bool_of_string>> <<string_of_bool>> (regex <<Regex.bool>>)

macro list e =
  conv <<Gen.to_list>> <<Gen.of_list>> (rep e)

static terminated_list ~sep e = list (e <* sep)
macro separated_list ~sep e =
  let e = opt (e <&> list (sep *> e)) in
  let to_ = <<function None -> [] | Some (h, t) -> (h :: t)>>
  and from_ = <<function [] -> None | h :: t -> Some (h, t)>>
  in
  conv to_ from_ e


(** {2 Witness} *)

(** A witness is a string such that [exec (compile re) (witness re) = true].
    The computation of the witness is deterministic and should result in
    a small example.

    It is used in [eval] for the part of the regex that are ignored.
*)

(* static rec witnesspp *)
(*   : type a . Format.formatter -> a t -> unit *)
(*   = fun ppf tre -> let open T in match tre with *)
(*     | Regexp (re, _) -> ~Format.pp_print_string ppf (R.witness re) *)
(*     | Conv (tre, _) -> witnesspp ppf tre *)
(*     | Opt _ -> () *)
(*     | Alt (tre1, _) -> witnesspp ppf tre1 *)
(*     | Seq (tre1, tre2) -> *)
(*       witnesspp ppf tre1 ; *)
(*       witnesspp ppf tre2 *)
(*     | Prefix (tre1,tre2) -> *)
(*       witnesspp ppf tre1 ; *)
(*       witnesspp ppf tre2 *)
(*     | Suffix (tre1,tre2) -> *)
(*       witnesspp ppf tre1 ; *)
(*       witnesspp ppf tre2 *)
(*     | Rep _ -> () *)
(*     | Mod (_,tre) -> *)
(*       witnesspp ppf tre *)

(** {2 Evaluation functions} *)

(** Evaluation is the act of filling the holes. *)

(* static pstr = ~Format.pp_print_string *)
(* static rec pprep f ppf gen = match gen () with *)
(*   | None -> () *)
(*   | Some x -> f ppf x ; pprep f ppf gen *)

(* static rec evalpp *)
(*   : type a . a t -> (Format.formatter -> a -> unit) expr *)
(*   = fun tre -> let open T in match tre with *)
(*     | Regexp (_, lazy cre) -> << fun ppf v -> *)
(*         if not @@ Re.execp cre v then *)
(*           invalid_arg @@ *)
(*           ~Printf.sprintf "Tyre.eval: regexp not respected by \"%s\"." v ; *)
(*         pstr ppf v *)
(*       >> *)
(*     | Conv (tre, conv) -> << fun ppf v -> evalpp tre ppf (conv.from_ v) >> *)
(*     | Opt p -> begin function *)
(*         | None -> pstr ppf "" *)
(*         | Some x -> evalpp p ppf x *)
(*       end *)
(*     | Seq (tre1,tre2) -> fun (x1, x2) -> *)
(*       evalpp tre1 ppf x1 ; *)
(*       evalpp tre2 ppf x2 ; *)
(*     | Prefix(tre_l,tre) -> *)
(*       fun v -> witnesspp ppf tre_l ; evalpp tre ppf v *)
(*     | Suffix(tre,tre_g) -> *)
(*       fun v -> evalpp tre ppf v ; witnesspp ppf tre_g *)
(*     | Alt (treL, treR) -> begin function *)
(*         | `Left x -> evalpp treL ppf x *)
(*         | `Right x -> evalpp treR ppf x *)
(*       end *)
(*     | Rep tre -> *)
(*       pprep (evalpp tre) ppf *)
(*     | Mod (_, tre) -> evalpp tre ppf *)

(* static eval tre = Format.asprintf "%a" (evalpp tre) *)

(** {2 matching} *)

(** {3 Regexp construction}

    In order to record how we constructed the regexp and how to later
    extract information, we build a witness containing all the tools we need.

    Each alternative is marked with {!Re.mark}. We store the markid in order
    to be able to guess the branch matched.
*)

macro map_mark (x,y,z) =
  let a = <<Re.mark $z>> in
  (x, y, <<fst $a>>, <<snd $a>>)


macro rec build
  : type a. int -> a t -> int * a T.wit * Re.t expr
  = let open! Re in let open T in
  let (+) = ~Pervasives.(+) in
  let (@!) = ~Pervasives.(@@) in 
  fun i -> function
    | Regexp (re, _) ->
      (i+1), Lit i, << group @@ no_group $re >>
    | Conv (e, conv) ->
      let i', w, re = build i e in
      i', Conv (w, conv), re
    | Opt e ->
      let i', w, id, re = map_mark @! build i e in
      i', Opt (id,w), <<opt $re>>
    | Alt (e1,e2) ->
      let i', w1, id1, re1 = map_mark @! build i e1 in
      let i'', w2, re2 = build i' e2 in
      i'', Alt (id1, w1, w2), <<alt [$re1 ; $re2]>>
    | Prefix (e_ign,e) ->
      let i', w, re = build i e in
      let _, _, re_ign = build 1 e_ign in
      i', w, <<seq [no_group $re_ign ; $re]>>
    | Suffix (e,e_ign) ->
      let i', w, re = build i e in
      let _, _, re_ign = build 1 e_ign in
      i', w, << seq [$re ; no_group $re_ign] >>
    | Seq (e1,e2) ->
      let i', w1, re1 = build i e1 in
      let i'', w2, re2 = build i' e2 in
      i'', Seq (w1, w2), << seq [$re1; $re2] >>
    | Rep e ->
      let _, w, re = build 1 e in
      (i+1), Rep (i,w,<<Re.compile $re>>), << group @@ rep @@ no_group $re >>
    | Mod (f, e) ->
      let i', w, re = build i e in
      i', w, << $f $re >>

(** {3 Extraction.} *)

(** Extracting is just a matter of following the witness.
    We just need to take care of counting where we are in the matching groups.

    To avoid copy, we pass around the original string (and we use positions).
*)
macro rec extract
  : type a. original:(string expr) -> a T.wit -> Re.substrings expr -> a expr
  = fun ~original rea s -> let open T in match rea with
    | Lit i -> << Re.get $s $(Expr.of_int i) >>
    | Conv (w, conv) ->
      let code = extract ~original w s in
      << $(conv.to_) $code >>
    | Opt (id,w) ->
      let code = extract ~original w s in <<
        if not @@ Re.marked $s $id then None
        else Some $code
      >>
    | Alt (i1,w1,w2) ->
      let code1 = extract ~original w1 s in
      let code2 = extract ~original w2 s in
      << if Re.marked $s $i1 then
          `Left $code1
        else
          (* Invariant: Alt produces [Re.alt [e1 ; e2]] *)
          `Right $code2
      >>
    | Seq (e1,e2) ->
      let code1 = extract ~original e1 s in
      let code2 = extract ~original e2 s in
      << ($code1, $code2) >>
    | Rep (i,e,re) -> extract_list ~original e re i s

(** We need to re-match the string for lists, in order to extract
    all the elements.
    Re doesn't offer the possibility to keep the results when
    grouping under a star (one could argue it's theoretically not
    possible as it would be equivalent to counting in an automaton).
*)
and extract_list
  : type a. original:string expr -> a T.wit -> Re.re expr -> int -> Re.substrings expr -> a gen expr
  = fun ~original e re i s ->
    let aux = << fun subs -> $(extract ~original e <<subs>>) >> in
    <<
      let (pos, pos') = Re.get_ofs $s $(Expr.of_int i) in
      let len = pos' - pos in
      Gen.map $aux @@ Re.all_gen ~pos ~len $re $original
    >>

(** {4 Multiple match} *)

type +'r route = Route : 'a t * ('a -> 'r) expr -> 'r route

static route re f = Route (re, f)

static (-->) = route

type 'r wit_route =
    WRoute : Re.markid expr * 'a T.wit * ('a -> 'r) expr -> 'r wit_route

(* It's important to keep the order here, since Re will choose
   the first regexp if there is ambiguity.
*)
macro rec build_route_aux i rel wl = function
  | [] -> << List.rev $rel >>, ~List.rev wl
  | Route (tre, f) :: l ->
    let i', wit, id, re = map_mark (build i tre) in
    let w = WRoute (id, wit, f) in
    build_route_aux i' << $re :: $rel >> (w::wl) l

macro build_route l = build_route_aux 1 <<[]>> [] l

macro rec extract_route ~original wl subs = match wl with
  | [] ->
    (* Invariant: At least one of the regexp of the alternative matches. *)
    assert false
  | WRoute (id, wit, f) :: wl ->
    let code = extract ~original wit subs in
    let code_rest = extract_route ~original wl subs in
    << if Re.Mark.test $subs $id
      then $f $code
      else $code_rest
    >>

(** {4 Compilation and execution} *)

type 'r info =
  | One of 'r T.wit
  | Routes of 'r wit_route list

type 'a re = { info : 'a info ; cre : Re.re expr }

macro compile tre =
  let _, wit, re = build 1 tre in
  let cre = << Re.compile $re >> in
  { info = One wit ; cre }

macro route l =
  let rel, wl = build_route l in
  let cre = << Re.compile @@ Re.alt $rel >> in
  { info = Routes wl ; cre }


type 'a error = [
  | `NoMatch of string
  | `ConverterFailure of exn
]

macro exec { info ; cre } =
  let f original subs = match info with
    | One wit -> extract ~original wit subs
    | Routes wl -> extract_route ~original wl subs
  in
  << fun ?pos ?len original ->
    match Re.exec_opt ?pos ?len $cre original with
    | None -> Result.Error (`NoMatch original)
    | Some subs ->
      try
        Result.Ok $(f <<original>> <<subs>>)
      with ConverterFailure exn ->
        Result.Error (`ConverterFailure exn) >>

macro execp {cre ; _ } =
  << fun ?pos ?len s -> Re.execp ?pos ?len $cre s >>

(** Pretty printers *)

(* static sexp ppf s fmt = Format.fprintf ppf ("@[<3>(%s@ "^^fmt^^")@]") s *)

(* (\* Only in the stdlib since 4.02, so we copy. *\) *)
(* static rec pp_list pp ppf = function *)
(*   | [] -> () *)
(*   | [v] -> pp ppf v *)
(*   | v :: vs -> *)
(*     pp ppf v; *)
(*     Format.pp_print_space ppf (); *)
(*     pp_list pp ppf vs *)

(* static rec pp *)
(*   : type a. _ -> a t -> unit *)
(*   = fun ppf -> let open T in function *)
(*   | Regexp (re,_) -> sexp ppf "Re" "%a" Re.pp re *)
(*   | Conv (tre,_) -> sexp ppf "Conv" "%a" pp tre *)
(*   | Opt tre -> sexp ppf "Opt" "%a" pp tre *)
(*   | Alt (tre1, tre2) -> sexp ppf "Alt" "%a@ %a" pp tre1 pp tre2 *)
(*   | Seq (tre1 ,tre2) -> sexp ppf "Seq" "%a@ %a" pp tre1 pp tre2 *)
(*   | Prefix (tre1, tre2) -> *)
(*     sexp ppf "Prefix" "%a@ %a" pp tre1 pp tre2 *)
(*   | Suffix (tre1, tre2) -> *)
(*     sexp ppf "Suffix" "%a@ %a" pp tre1 pp tre2 *)
(*   | Rep tre -> sexp ppf "Rep" "%a" pp tre *)
(*   | Mod (_,tre) -> sexp ppf "Mod" "%a" pp tre *)

(* static rec pp_wit *)
(*   : type a. _ -> a T.wit -> unit *)
(*   = fun ppf -> let open T in function *)
(*   | Lit i -> sexp ppf "Lit" "%i" i *)
(*   | Conv (tre,_) -> sexp ppf "Conv" "%a" pp_wit tre *)
(*   | Opt (_, tre) -> sexp ppf "Opt" "%a" pp_wit tre *)
(*   | Alt (_, tre1, tre2) -> sexp ppf "Alt" "%a@ %a" pp_wit tre1 pp_wit tre2 *)
(*   | Seq (tre1 ,tre2) -> sexp ppf "Seq" "%a@ %a" pp_wit tre1 pp_wit tre2 *)
(*   | Rep (i, w, re) -> sexp ppf "Rep" "%i@ %a@ %a" i pp_wit w Re.pp_re re *)

(* static pp_wit_route *)
(*   : type a. _ -> a wit_route -> unit *)
(*   = fun ppf (WRoute (_,w,_)) -> pp_wit ppf w *)

(* static pp_re ppf = function *)
(*   | { info = One w; cre } -> *)
(*     sexp ppf "One" "%a@ %a" Re.pp_re cre pp_wit w *)
(*   | { info = Routes wl; cre } -> *)
(*     sexp ppf "Route" "%a@ %a" Re.pp_re cre (pp_list pp_wit_route) wl *)

(* module Internal = struct *)
(*   include T *)

(*   static to_t x = x *)
(*   static from_t x = x *)

(*   exception ConverterFailure = ConverterFailure *)

(*   static build = build *)
(*   static extract = extract *)
(* end *)
