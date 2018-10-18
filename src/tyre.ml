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

open Tyrefun

include Make(struct
    include Re

    type string = String.t
    let pp_string = Format.pp_print_string

    let exec = exec_opt
    let all = Seq.all
    let test = execp

    module Idx = Impl.MarkIdx
  end)

let pcre s = regex @@ Re.Pcre.re s

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
