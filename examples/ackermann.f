let ack = fix λf.λx.λy.if0 x (succ y) (if0 y (f (pred x) (succ 0)) (f (pred x) (f x (pred y)))) in ack (succ (succ (succ 0))) (succ 0) end
