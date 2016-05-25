
let map_snd f (x,y) = (x, f y)

(** {2 The various types} *)

type ('a, 'b) conv = {
  to_ : 'a -> 'b ;
  from_ : 'b -> 'a ;
}

type 'a gen = unit -> 'a option

type 'a t =
  (* We store a compiled regex to efficiently check string when unparsing. *)
  | Regexp : Re.t * Re.re Lazy.t -> string t
  | Conv   : 'a t * ('a, 'b) conv -> 'b t
  | Opt    : 'a t -> ('a option) t
  | Alt    : 'a t * 'b t -> [`Left of 'a | `Right of 'b] t
  | Seq    : 'a t * 'b t -> ('a * 'b) t
  | Prefix : _ t * string * 'a t -> 'a t
  | Suffix : 'a t * string * _ t  -> 'a t
  | Rep   : 'a t -> 'a gen t

let regex x =
  let re = lazy Re.(compile @@ whole_string @@ no_group x) in
  Regexp (x, re)
let conv to_ from_ x = Conv (x, {to_; from_})

let seq a b = Seq (a, b)
let alt a b = Alt (a, b)

let (<?>) = alt
let (<*>) = seq

let prefix (x,s) a = Prefix (x, s, a)
let suffix a (x,s) = Suffix (a, s, x)
let prefixstr s a = prefix (regex (Re.str s), s) a
let suffixstr a s = suffix a (regex (Re.str s), s)
let opt a = Opt a

let ( *>) = prefixstr
let (<* ) = suffixstr
let ( **>) = prefix
let (<** ) = suffix

let rep x = Rep x
let rep1 x = x <*> rep x

module Regex = struct
  open Re

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

let pos_int =
  conv int_of_string string_of_int (regex Regex.pos_int)

let int =
  conv int_of_string string_of_int (regex Regex.int)

let float =
  conv float_of_string string_of_float (regex Regex.float)

let bool =
  conv bool_of_string string_of_bool (regex Regex.bool)

let list e =
  conv Gen.to_list Gen.of_list (rep e)

let terminated_list ~sep e = list (e <* sep)
let separated_list ~sep e =
  let e = opt (e <*> list (sep *> e)) in
  let to_ = function None -> [] | Some (h, t) -> h :: t
  and from_ = function [] -> None | h :: t -> Some (h, t)
  in
  conv to_ from_ e

(** {2 Evaluation functions} *)

(** Evaluation is the act of filling the holes. *)

let pstr = Format.pp_print_string
let rec pprep f ppf gen = match gen () with
  | None -> ()
  | Some x -> f ppf x ; pprep f ppf gen

let rec evalpp
  : type a . a t -> Format.formatter -> a -> unit
  = fun tre ppf -> match tre with
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
    | Prefix(_,s,tre) ->
      fun v -> pstr ppf s ; evalpp tre ppf v
    | Suffix(tre,s,_) ->
      fun v -> evalpp tre ppf v ; pstr ppf s
    | Alt (treL, treR) -> begin function
        | `Left x -> evalpp treL ppf x
        | `Right x -> evalpp treR ppf x
      end
    | Rep tre ->
      pprep (evalpp tre) ppf

let eval tre = Format.asprintf "%a" (evalpp tre)

(** {2 matching} *)

(** {3 Regexp construction}

    In order to record how we constructed the regexp and how to later
    extract information, we build a witness containing all the tools we need.

    Each alternative is marked with {!Re.mark}. We store the markid in order
    to be able to guess the branch matched.
*)

type _ wit =
  | Regexp : Re.t -> string wit
  | Conv   : 'a wit * ('a, 'b) conv -> 'b wit
  | Opt    : Re.markid * 'a wit -> 'a option wit
  | Alt    : Re.markid * 'a wit * Re.markid * 'b wit
    -> [`Left of 'a | `Right of 'b] wit
  | Seq    :
      'a wit * 'b wit -> ('a * 'b) wit
  | Rep   : 'a wit * Re.re -> 'a gen wit

(** Count the matching groups the regexp encoding some atom will have. *)
let rec count_group
  : type a. a wit -> int
  = function
    | Regexp _ -> 1
    | Conv (e, _) -> count_group e
    | Opt (_,e) -> count_group e
    | Alt (_,e1,_,e2) -> count_group e1 + count_group e2
    | Seq (e1,e2) -> count_group e1 + count_group e2
    | Rep _ -> 1

let incrg e i = i + count_group e


let rec build
  : type a. a t -> a wit * Re.t
  = let open Re in function
    | Regexp (re, _) -> Regexp re, group @@ no_group re
    | Conv (e, conv) ->
      let w, re = build e in
      Conv (w, conv), re
    | Opt e ->
      let w, (id, re) = map_snd mark @@ build e in
      Opt (id,w), alt [epsilon ; re]
    | Alt (e1,e2) ->
      let w1, (id1, re1) = map_snd mark @@ build e1 in
      let w2, (id2, re2) = map_snd mark @@ build e2 in
      Alt (id1, w1, id2, w2), alt [re1 ; re2]
    | Prefix (e_ign,_,e) ->
      let w, re = build e in
      let _, re_ign = build e_ign in
      w, seq [no_group re_ign ; re]
    | Suffix (e,_,e_ign) ->
      let w, re = build e in
      let _, re_ign = build e_ign in
      w, seq [re ; no_group re_ign]
    | Seq (e1,e2) ->
      let w1, re1 = build e1 in
      let w2, re2 = build e2 in
      Seq (w1, w2), seq [re1; re2]
    | Rep e ->
      let w, re = build e in
      Rep (w,Re.compile re), group @@ rep @@ no_group re

(** {3 Extraction.} *)

(** Extracting is just a matter of following the witness.
    We just need to take care of counting where we are in the matching groups.
*)
let rec extract_aux
  : type a. a wit -> int -> Re.substrings -> int * a
  = fun rea i s -> match rea with
    | Regexp _ -> incrg rea i, Re.get s i
    | Conv (w, conv) ->
      let i, v = extract_aux w i s in
      i, conv.to_ v
    | Opt (id,w) ->
      if not @@ Re.marked s id then incrg rea i, None
      else map_snd (fun x -> Some x) @@ extract_aux w i s
    | Alt (i1,w1,id2,w2) ->
      if Re.marked s i1 then
        map_snd (fun x -> `Left x) @@ extract_aux w1 i s
      else if Re.marked s id2 then
        map_snd (fun x -> `Right x) @@ extract_aux w2 (incrg w1 i) s
      else
        (* Invariant: Alt produces [Re.alt [e1 ; e2]] *)
        assert false
    | Seq (e1,e2) ->
      let i, v1 = extract_aux e1 i s in
      let i, v2 = extract_aux e2 i s in
      i, (v1, v2)
    | Rep (e,re) -> i+1, extract_list e re i s

and extract
  : type a . a wit -> Re.substrings -> a
  = fun e s -> snd @@ extract_aux e 1 s

(** We need to re-match the string for lists, in order to extract
    all the elements.
    Re doesn't offer the possibility to keep the results when
    grouping under a star (one could argue it's theoretically not
    possible as it would be equivalent to counting in an automaton).
*)
and extract_list
  : type a. a wit -> Re.re -> int -> Re.substrings -> a gen
  = fun e re i s ->
    let aux = extract e in
    let (pos, pos') = Re.get_ofs s i in
    let len = pos' - pos in
    (* The whole original string, no copy! *)
    let original = Re.get s 0 in
    Gen.map aux @@ Re.all_gen ~pos ~len re original

(** {4 Multiple match} *)

type +'r route = Route : 'a t * ('a -> 'r) -> 'r route

let route re f = Route (re, f)

let (-->) = route

type 'r wit_route =
    WRoute : Re.markid * 'a wit * ('a -> 'r) -> 'r wit_route

(* It's important to keep the order here, since Re will choose
   the first regexp if there is ambiguity.
*)
let rec build_route_aux rel wl = function
  | [] -> List.rev rel, List.rev wl
  | Route (tre, f) :: l ->
    let wit, re = build tre in
    let id, re = Re.mark re in
    let w = WRoute (id, wit, f) in
    build_route_aux (re::rel) (w::wl) l

let build_route l = build_route_aux [] [] l

let rec extract_route subs = function
  | [] ->
    (* Invariant: At least one of the regexp of the alternative matches. *)
    assert false
  | WRoute (id, wit, f) :: l ->
    if Re.marked subs id then f @@ extract wit subs
    else extract_route subs l


(** {4 Compilation and execution} *)

type 'r info =
  | One of 'r wit
  | Routes of 'r wit_route list

type 'a re = { info : 'a info ; cre : Re.re }

let compile tre =
  let wit, re = build tre in
  let cre = Re.(compile re) in
  { info = One wit ; cre }

let route l =
  let rel, wl = build_route l in
  let cre = Re.(compile @@ alt rel) in
  { info = Routes wl ; cre }

let exec ?pos ?len { info ; cre } s =
  match Re.exec_opt ?pos ?len cre s with
  | None -> None
  | Some subs -> match info with
    | One wit -> Some (extract wit subs)
    | Routes wl -> Some (extract_route subs wl)
