let add = fix λf.λn.λm.if0 n m (f (pred n) (succ m)) in
let fib = fix λf.λn.if0 n (succ 0)
                        (if0 (pred n) (succ 0)
                             (add (f (pred n)) (f (pred (pred n))))) in
fib (succ (succ (succ 0))) end end
