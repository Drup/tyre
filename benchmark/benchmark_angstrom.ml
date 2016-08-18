[@@@ocaml.warning "-40-44-48"]

let re = Tyre.compile Tyre.(whole_string @@ rep RFC2616.request)
let tyre s =
  match Tyre.exec re s with
  | Result.Ok l -> assert (Gen.length l = 55 * 100)
  | Result.Error _ -> failwith "oups"

let tyre_test s =
  assert (Tyre.execp re s)

let re2 = Tyre.compile RFC2616.request
let tyre_all s =
  match Tyre.all_gen re2 s with
  | Result.Ok l -> assert (Gen.length l = 55 * 100)
  | Result.Error _ -> failwith "oups"

let angstrom s =
  match Angstrom.(parse_only (many Angstrom_rFC2616.request)) (`String s) with
  | Result.Ok l -> assert (List.length l = 55 * 100)
  | Result.Error _ -> failwith "oups"



let oc = open_in "benchmark/data/http-requests.txt.100"
let s = CCIO.read_all oc

let l = [
  "tyre", tyre ;
  "tyre.all", tyre_all ;
  "tyre.test", tyre_test ;
  "angstrom", angstrom ;
]



(** Utilities *)

let benchs =
  let get_sample (name, f) =
    name, lazy (Benchmark.latency1 ~style:Nil ~name ~repeat:4 50L f s)
  in
  List.map get_sample l

let tree =
  let open Benchmark.Tree in
  let f (n,s) = n @> s in
  concat @@ List.map f benchs

let samples () =
  let f (_,s) l =
    if Lazy.is_val s then
      Benchmark.merge (Lazy.force s) l
    else l
  in
  List.fold_right f benchs []

let () =
  let open Benchmark in
  let tree = Tree.("http" @>> tree) in
  Tree.register tree ;
  Tree.run_global () ;
  let l = samples () in
  if l <> [] then tabulate l
