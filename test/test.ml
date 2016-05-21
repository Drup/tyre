open Tyre

let r =
  "foo" *> separated_list ~sep:"," int <* "bar"

let test1 () =
  Alcotest.(check @@ option @@ list int)
    "1 elem" (get r "foo3bar") @@ Some [3]

let l = [
  "int list", `Quick, test1
]

let () = Alcotest.run "tyre" [
    "test", l
  ]
