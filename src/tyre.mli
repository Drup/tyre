type 'a t

val regex : Re.t -> string t
val conv : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

val opt : 'a t -> 'a option t
val rep : 'a t -> 'a list t
val rep1 : 'a t -> ('a * 'a list) t

val alt : 'a t -> 'b t -> [`Left of 'a | `Right of 'b] t
val seq : 'a t -> 'b t -> ('a * 'b) t
val prefix : string -> 'a t -> 'a t
val suffix : 'a t -> string -> 'a t

val (<?>) : 'a t -> 'b t -> [`Left of 'a | `Right of 'b] t
val (<*>) : 'a t -> 'b t -> ('a * 'b) t
val (<* ) : 'a t -> string -> 'a t
val ( *>) :  string -> 'a t -> 'a t

val int : int t
val pos_int : int t
val float : float t
val bool : bool t

val terminated_list : sep:string -> 'a t -> 'a list t
val separated_list : sep:string -> 'a t -> 'a list t

val eval : 'a t -> 'a -> string

val get : 'a t -> string -> 'a option

type 'r route = Route : 'a t * ('a -> 'r) -> 'r route
val (-->) : 'a t -> ('a -> 'r) -> 'r route

val match_ :
  ?default:(string -> 'r) -> 'r route list -> string -> 'r
