module type S = sig
  type t
  val pp : Format.formatter -> t -> unit

  val whole_string : t -> t
  val no_group : t -> t
  val group : t -> t
  val seq : t list -> t
  val alt : t list -> t
  val opt : t -> t
  val rep : t -> t
  
  (** Compilation *)
  type re
  val pp_re : Format.formatter -> re -> unit
  val compile : t -> re

  (** Witness *)
  type string
  val pp_string : Format.formatter -> string -> unit
  val witness : t -> string

  (** Matching *)
  module Group : sig
    type t
    val get : t -> int -> string
    val offset : t -> int -> int * int
  end

  module Idx : sig
    type idx
    val with_mark :
      (int -> 'b -> 'c * 'd * t) -> int -> 'b -> 'c * 'd * idx * t
    val test : Group.t -> idx -> bool
  end

  val exec : ?pos:int -> ?len:int -> re -> string -> Group.t option
  val all : ?pos:int -> ?len:int -> re -> string -> Group.t Seq.t
  val test : ?pos:int -> ?len:int -> re -> string -> bool
end
