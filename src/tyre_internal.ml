(** {2 The various types} *)

module T = struct

  type ('a, 'b) conv = {
    to_ : ('a -> 'b) code ;
    from_ : ('b -> 'a) code ;
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
    | Rep   : int * 'a wit * Re.re code -> 'a Seq.t wit

end
