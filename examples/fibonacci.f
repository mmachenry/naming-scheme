let add = fix λf.λn.λm.
  if n
  then m
  else f (pred n) (succ m)
in let fib = fix λf.λn.
  if n
  then succ zero
  else if pred n
       then succ zero 
       else add (f (pred n)) (f (pred (pred n)))
in fib (succ (succ (succ zero)))
