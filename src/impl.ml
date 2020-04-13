module MarkIdx = struct
  type idx = Re.Mark.t
  let with_mark f i re =
    let i, w, re = f i re in
    let idx, re = Re.mark re in
    i, w, idx, re
  let test = Re.Mark.test
end

module GroupIdx = struct
  type idx = int
  let with_mark f i re =
    let i', w, re = f (i+1) re in
    let re = Re.group re in
    i', w, i, re
  let test = Re.Group.test
end
