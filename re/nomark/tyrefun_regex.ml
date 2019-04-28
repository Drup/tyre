include Re

type string = String.t
let pp_string = Format.pp_print_string

let exec = exec_opt
let all = Seq.all
let test = execp

module Idx = struct
  type idx = int
  let with_mark f i re =
    let i', w, re = f (i+1) re in
    let re = Re.group re in
    i', w, i, re
  let test = Re.Group.test
end
