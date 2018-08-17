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

(** Tyre

    Typed regular expressions.
*)

type 'a t
(** A typed regular expression.

    The type variable is the type of the returned value when the typed regular expression (tyregex) is executed. tyregexs are bi-directional and can be used both for {{!matching}matching} and {{!eval}evaluation}. Multiple tyregexs can be combined in order to do {{!routing}routing} in similar manner as switches/pattern matching.

    Typed regular expressions are strictly as expressive as regular expressions from {{:https://github.com/ocaml/ocaml-re}re} (and are, as such, {b regular} expressions, not PCREs). Performances should be exactly the same.

    For example [tyre : int t] can be used to return an [int]. In the rest of the documentation, we will use [tyre] to designate a value of type {!t}.
*)

(** {2 Combinators} *)

val pcre : string -> string t
(** [pcre s] is a tyregex that matches the PCRE [s] and return the 
    corresponding string.
    Groups in [s] are ignored.
*)

val regex : Re.t -> string t
(** [regex re] is a tyregex that matches [re] and return the corresponding string.
    Groups inside [re] are erased.
*)

val conv : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t
(** [conv to_ from_ tyre] matches the same text as [tyre], but converts back and forth to a different data type.

    [to_] is allowed to raise an exception [exn].
    In this case, {!exec} will return [`ConverterFailure exn].

For example, this is the implementation of {!pos_int}:

{[
let pos_int =
  Tyre.conv
    int_of_string string_of_int
    (Tyre.regex (Re.rep1 Re.digit))
]}
*)

val opt : 'a t -> 'a option t
(** [opt tyre] matches either [tyre] or the empty string. Similar to {!Re.opt}. *)

val alt : 'a t -> 'b t -> [`Left of 'a | `Right of 'b] t
(** [alt tyreL tyreR] matches either [tyreL] (and will then return [`Left v]) or [tyreR] (and will then return [`Right v]).
*)

(** {3 Repetitions} *)

val rep : 'a t -> 'a Seq.t t
(** [rep tyre] matches [tyre] zero or more times. Similar to {!Re.rep}.

    For {{!matching}matching}, [rep tyre] will matches the string a first time, then [tyre] will be used to walk the matched part to extract values.
*)

val rep1 : 'a t -> ('a * 'a Seq.t) t
(** [rep1 tyre] is [seq tyre (rep tyre)]. Similar to {!Re.rep1}. *)

(** {3 Sequences} *)

val seq : 'a t -> 'b t -> ('a * 'b) t
(** [seq tyre1 tyre2] matches [tyre1] then [tyre2] and return both values. *)

val prefix : _ t -> 'a t -> 'a t
(** [prefix tyre_i tyre] matches [tyre_i], ignores the result, and then matches [tyre] and returns its result. Converters in [tyre_i] are never called.
*)

val suffix : 'a t -> _ t -> 'a t
(** Same as [prefix], but reversed. *)



(** {3 Infix operators} *)

val (<|>) : 'a t -> 'b t -> [`Left of 'a | `Right of 'b] t
(** [t <|> t'] is [alt t t']. *)

val (<&>) : 'a t -> 'b t -> ('a * 'b) t
(** [t <&> t'] is [seq t t']. *)

val ( *>) : _ t -> 'a t -> 'a t
(** [ ti *> t ] is [prefix ti t]. *)

val (<* ) : 'a t -> _ t -> 'a t
(** [ t <* ti ] is [suffix t ti]. *)

module Infix : sig

  val (<|>) : 'a t -> 'b t -> [`Left of 'a | `Right of 'b] t
  (** [t <|> t'] is [alt t t']. *)

  val (<&>) : 'a t -> 'b t -> ('a * 'b) t
  (** [t <&> t'] is [seq t t']. *)

  val ( *>) : _ t -> 'a t -> 'a t
  (** [ ti *> t ] is [prefix ti t]. *)

  val (<* ) : 'a t -> _ t -> 'a t
  (** [ t <* ti ] is [suffix t ti]. *)

end

(** {3 Useful combinators} *)

val str : string -> unit t
(** [str s] matches [s] and evaluates to [s]. *)

val char : char -> unit t
(** [char c] matches [c] and evaluates to [c]. *)

val blanks : unit t
(** [blanks] matches [Re.(rep blank)] and doesn't return anything. *)

val int : int t
(** [int] matches [-?[0-9]+] and returns the matched integer.

    Integers that do not fit in an [int] will fail.
*)

val pos_int : int t
(** [pos_int] matches [[0-9]+] and returns the matched positive integer.

    Integers that do not fit in an [int] will fail.
*)

val float : float t
(** [float] matches [-?[0-9]+( .[0-9]* )?] and returns the matched floating point number.

    Floating point numbers that do not fit in a [float] returns {!infinity} or {!neg_infinity}.
*)

val bool : bool t
(** [bool] matches [true|false] and returns the matched boolean. *)

val list : 'a t -> 'a list t
(** [list e] is similar to [rep e], but returns a list. *)

val terminated_list : sep:_ t -> 'a t -> 'a list t
(** [terminated_list ~sep tyre] is [ list (tyre <* sep) ]. *)

val separated_list : sep:_ t -> 'a t -> 'a list t
(** [separated_list ~sep tyre] is equivalent to [opt (e <&> list (sep *> e))]. *)

(** {3 Other combinators}

    See {!Re} for details on the semantics of those combinators. *)

val start : unit t
val stop : unit t

val word : 'a t -> 'a t
val whole_string : 'a t -> 'a t
val longest : 'a t -> 'a t
val shortest : 'a t -> 'a t
val first : 'a t -> 'a t
val greedy : 'a t -> 'a t
val non_greedy : 'a t -> 'a t
val nest : 'a t -> 'a t

(** {2:matching Matching} *)

type 'a re
(** A compiled typed regular expression. *)

val compile : 'a t -> 'a re
(** [compile tyre] is the compiled tyregex representing [tyre].
*)

type 'a error = [
  | `NoMatch of 'a re * string
  | `ConverterFailure of exn
]

val pp_error : Format.formatter -> _ error -> unit

val exec : ?pos:int -> ?len:int -> 'a re -> string -> ('a, 'a error) Result.result
(** [exec ctyre s] matches the string [s] using
    the compiled tyregex [ctyre] and returns the extracted value.

    Returns [Error (`NoMatch (tyre, s)] if [tyre] doesn't match [s].
    Returns [Error (`ConverterFailure exn)] if a converter failed with the exception [exn].

    @param pos optional beginning of the string (default 0)
    @param len length of the substring of [str] that can be matched (default to the end of the string)
*)

val execp : ?pos:int -> ?len:int -> 'a re -> string -> bool
(** [execp ctyre s] returns [true] if [ctyre] matches [s]. Converters
    are never called.

    @param pos optional beginning of the string (default 0)
    @param len length of the substring of [str] that can be matched (default to the end of the string)

    @since 0.1.1
*)


(** {3:repeat Repeated Matching} *)

val all : ?pos:int -> ?len:int -> 'a re -> string -> ('a list, 'a error) Result.result
(** [all ctyre s] calls to {!exec} repeatedly and returns the list of all the matches. *)

val all_seq : ?pos:int -> ?len:int -> 'a re -> string -> 'a Seq.t
(** [all_seq ctyre s] is [all ctyre s] but returns a {!gen} instead. Matches
    are enumerated lazily.

    Exceptions raised by converters are not caught.
*)

(** {3:routing Routing} *)

type +'a route = Route : 'x t * ('x -> 'a) -> 'a route
(** A route is a pair of a tyregex and a handler.
    When the tyregex is matched, the function is called with the
    result of the matching.
*)

val (-->) : 'x t -> ('x -> 'a) -> 'a route
(** [tyre --> f] is [Route (tyre, f)]. *)

val route : 'a route list -> 'a re
(** [route [ tyre1 --> f1 ; tyre2 --> f2 ]] produces a compiled
    tyregex such that, if [tyre1] matches, [f1] is called, and so on.

    The compiled tyregex shoud be used with {!exec}.
*)


(** {2:eval Evaluating} *)

val eval : 'a t -> 'a -> string
(** [eval tyre v] returns a string [s] such that [exec (compile tyre) s = v].

    Note that such string [s] is not unique. [eval] will usually returns a very simple witness. *)

val evalpp : 'a t -> Format.formatter -> 'a -> unit
(** [evalpp tyre ppf v] is equivalent to [Format.fprintf ppf "%s" (eval tyre v)], but more efficient.

    Is is generally used with ["%a"]:
{[
let my_pp = Tyre.evalpp tyre in
Format.printf "%a@." my_pp v
]}
*)

(** {2:pp Pretty printing} *)

val pp : Format.formatter -> 'a t -> unit

val pp_re : Format.formatter -> 'a re -> unit

(** Internal types *)
module Internal : sig

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

  val from_t : 'a t -> 'a raw
  val to_t : 'a raw -> 'a t

  type _ wit =
    | Lit    : int -> string wit
    | Conv   : 'a wit * ('a, 'b) conv -> 'b wit
    | Opt    : Re.markid * 'a wit -> 'a option wit
    | Alt    : Re.markid * 'a wit * 'b wit
      -> [`Left of 'a | `Right of 'b] wit
    | Seq    :
        'a wit * 'b wit -> ('a * 'b) wit
    | Rep   : int * 'a wit * Re.re -> 'a Seq.t wit

  val build : int -> 'a raw -> int * 'a wit * Re.t
  val extract : original:string -> 'a wit -> Re.substrings -> 'a

end
