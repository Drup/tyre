open Tyre.Infix

type section = string * (string * string) list

type ini =
  { named: section list
  ; anon: (string * string) list }

let take_till cset = Re.(rep (compl [set cset])) |> Tyre.regex
let strip = Tyre.conv String.trim (fun x -> x)

let section_title =
  let open! Tyre in
  let sec_name = "]" |> take_till |> strip in
  char '[' *> sec_name <* char ']'

let entry : (string * string) Tyre.t =
  let open Tyre in
  let equal = blanks *> char '=' <* blanks in
  let key = strip @@ take_till "[]=" in
  key <&> equal *> take_till "\n"

let empty_line =
  Re.( seq [rep (set " \t") ; char '\n'] |> rep )
  |> Tyre.regex

let line t =
  let open Tyre in
  t <* blanks <* char '\n'

let entry =
  empty_line *> entry <* empty_line

let section = Tyre.(terminated_list ~sep:(char '\n') entry)

let named_section =
  line section_title <&> section

let ini =
  Tyre.start *> section <&> Tyre.list named_section <* Tyre.stop
  |> Tyre.conv
    (fun (anon, named) -> { anon ; named })
    (fun { anon ; named } -> (anon, named))

let sample_ini =
  { anon=[ "lang", "OCaml"
         ; "lib", "foo bar" ]
  ; named =
      [ "lib",
        [ "re", "1.5.0"
        ; "tyre", "> 1.5.0" ]
      ; "src",
        [ "one", "two"
        ; "xxx", "foo bar baz" ]
      ; "lib_test",
        [ "alcotest", "*" ]
      ]
  }

let sample_ini_str = Tyre.eval ini sample_ini

let sample_ini' = Tyre.exec (Tyre.compile ini) sample_ini_str

let () =
  assert (Result.Ok sample_ini = sample_ini')
