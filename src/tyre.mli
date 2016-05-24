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

val regex : Re.t -> string t
(** [regex re] is a tyregex that matches [re] and return the corresponding string.
    Groups inside [re] are erased.
*)

val conv : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t
(** [conv to_ from_ tyre] matches the same text as [tyre], but converts back and forth to a different data type. For example, this is the implementation of {!pos_int}:

{[
let pos_int = Tyre.conv int_of_string string_of_int (Tyre.regex (Re.rep1 Re.digit))
]}
*)

val opt : 'a t -> 'a option t
(** [opt tyre] matches either [tyre] or the empty string. Similar to {!Re.opt}. *)

val rep : 'a t -> 'a list t
(** [rep tyre] matches [tyre] zero or more times. Similar to {!Re.rep}.

    For {{!matching}matching}, [rep tyre] will matches the string a first time, then [tyre] will be used to walk the matched part to extract values.
*)

val rep1 : 'a t -> ('a * 'a list) t
(** [rep1 tyre] is [seq tyre (rep tyre)]. Similar to {!Re.rep1}. *)

val alt : 'a t -> 'b t -> [`Left of 'a | `Right of 'b] t
(** [alt tyreL tyreR] matches either [tyreL] (and will then return [`Left v]) or [tyreR] (and will then return [`Right v]).
*)

val seq : 'a t -> 'b t -> ('a * 'b) t
(** [seq tyre1 tyre2] matches [tyre1] then [tyre2] and return both values. *)

val prefix : (_ t * string) -> 'a t -> 'a t
(** [prefix (tyre_i, s) tyre] matches [tyre_i], ignores the result, and then matches [tyre] and returns its result.

    [s] is the witness used for {{!eval}evaluation}. It is assumed (but not checked) that [tyre_i] matches [s].
*)

val prefixstr : string -> 'a t -> 'a t
(** [prefixstr s tyre] matches [s] then matches [tyre] and return its value.

    It is equal to [prefix (regex (Re.str s), s) tyre].
*)

val suffix : 'a t -> (_ t * string) -> 'a t
(** Same as [prefix], but reversed. *)

val suffixstr : 'a t -> string -> 'a t
(** Same as [prefixstr], but reversed. *)


(** {3 Infix operators}

    The tyregexs are on sides with an arrow.
*)

val (<?>) : 'a t -> 'b t -> [`Left of 'a | `Right of 'b] t
(** [t <?> t'] is [alt t t']. *)

val (<*>) : 'a t -> 'b t -> ('a * 'b) t
(** [t <?> t'] is [seq t t']. *)

val ( *>) :  string -> 'a t -> 'a t
(** [s *> t] is [prefixstr s t]. *)

val (<* ) : 'a t -> string -> 'a t
(** [t <* s] is [suffixstr s t]. *)

val ( **>) : (_ t * string) -> 'a t -> 'a t
(** [ (ti,s) **> t ] is [prefix (ti,s) t]. *)

val (<** ) : 'a t -> (_ t * string) -> 'a t
(** [ t <** (ti,s) ] is [suffix t (ti,s)]. *)

(** {3 Useful combinators} *)

val int : int t
(** [int] matches [-?[0-9]+] and returns the matched integer.

    Integers that do not fit in an [int] fails with [Failure "int_of_string"].
*)

val pos_int : int t
(** [pos_int] matches [[0-9]+] and returns the matched positive integer.

    Integers that do not fit in an [int] fails with [Failure "int_of_string"].
*)

val float : float t
(** [float] matches [-?[0-9]+( .[0-9]* )?] and returns the matched floating point number.

    Floating point numbers that do not fit in a [float] returns {!infinity} or {!neg_infinity}.
*)

val bool : bool t
(** [bool] matches [true|false] and returns the matched boolean. *)

val terminated_list : sep:string -> 'a t -> 'a list t
(** [terminated_list ~sep tyre] is [ rep (tyre <* sep) ]. *)

val separated_list : sep:string -> 'a t -> 'a list t
(** [separated_list ~sep tyre] is equivalent to [opt (e <*> rep (sep *> e))]. *)

(** {2:matching Matching} *)

type 'a re
(** A compiled typed regular expression. *)

val compile : 'a t -> 'a re
(** [compile tyre] is the compiled tyregex representing [tyre]. *)

val exec : ?pos:int -> ?len:int -> 'a re -> string -> 'a option
(** [exec ctyre s] matches the string [s] using the compiled tyregex [ctyre] and returns the extracted value.
*)

(** {3:routing Routing} *)

type +'a route = Route : 'x t * ('x -> 'a) -> 'a route
val (-->) : 'x t -> ('x -> 'a) -> 'a route

val route : 'a route list -> 'a re

(** {2:eval Evaluating} *)

val unparse : 'a t -> 'a -> string
(** [unparse tyre v] returns a string [s] such that [parse tyre s = v].

    Note that such string [s] is not unique. [unparse] will usually returns a very simple witness. *)

val unpparse : 'a t -> Format.formatter -> 'a -> unit
(** [unpparse tyre ppf v] is equivalent to [Format.fprintf ppf "%s" (unparse tyre v)], but more efficient.

    Is is generally used with ["%a"]:
{[
let my_pp = Tyre.unpparse tyre in
Format.printf "%a@." my_pp v
]}
*)
