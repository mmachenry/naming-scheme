let add = fix λf.λn.λm.if n then m else f (pred n) (succ m) in
let mul = fix λf.λn.λm.if n then zero else add m (f (pred n) m) in
let fact = fix λf.λn.if n then succ zero else mul n (f (pred n)) in
fact (succ (succ (succ zero)))
