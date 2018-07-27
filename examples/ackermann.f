let ack = fix λf.λx.λy.
  if x
  then succ y
  else if y
       then f (pred x) (succ zero)
       else f (pred x) (f x (pred y))
in ack (succ (succ (succ zero))) (succ zero)
