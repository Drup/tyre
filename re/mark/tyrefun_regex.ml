include Re

type string = String.t
let pp_string = Format.pp_print_string

let exec = exec_opt
let all = Seq.all
let test = execp

module Idx = struct
  type idx = Re.Mark.t
  let with_mark f i re =
    let i, w, re = f i re in
    let idx, re = Re.mark re in
    i, w, idx, re
  let test = Re.Mark.test
end
