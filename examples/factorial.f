let add = fix λf.λn.λm.if0 n m (f (pred n) (succ m)) in
let mul = fix λf.λn.λm.if0 n 0 (add m (f (pred n) m)) in
let fact = fix λf.λn.if0 n (succ 0) (mul n (f (pred n))) in
fact (succ (succ (succ 0))) end end end

