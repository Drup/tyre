[@@@ocaml.warning "-44"]
(* HTTP "parser", copied from angstrom. *)

open Tyre

module P = struct
  let space = Re.blank

  let eol = Re.set "\r\n"

  let token =
    Re.(rep1 @@ compl [
        rg '\000' '\031' ;
        set "\127)(<>@,;:\\/[]?={}"
      ])
end

let take_till re = regex @@ Re.(rep @@ compl [re])

let token = regex P.token
let digits = regex @@ Re.(rep1 digit)
let spaces = regex @@ Re.rep P.space

let lex p = p <* blanks

let version =
  str"HTTP/" *>
    seq
      (digits <* char '.')
      digits

let uri =
  take_till P.space

let meth = token
let eol = str"\r\n"

let request_first_line =
    lex meth <&> lex uri <&> version

let response_first_line =
    (lex version) <&>
    (lex (take_till P.space)) <&>
    (take_till P.eol)

let header =
  seq
    token
    (char ':' *> blanks *> take_till P.eol)


let request =
  let to_ (((meth, uri), version), headers) = (meth, uri, version, headers) in
  let of_ (meth, uri, version, headers) = (((meth, uri), version), headers) in
  conv to_ of_ @@ seq
    (request_first_line   <* eol)
    (rep (header <* eol) <* eol)

let response =
  let to_ (((version, status), msg), headers) = (version, status, msg, headers) in
  let of_ (version, status, msg, headers) = (((version, status), msg), headers) in
  conv to_ of_ @@ seq
    (response_first_line  <* eol)
    (rep (header <* eol) <* eol)
