let fact = fix λf.λn.if n then succ zero else n * f (pred n) in
fact (succ (succ (succ zero)))
