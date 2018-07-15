let col = fix lambda f. lambda n.
      if even n
      then div2 n
      else succ (mul3 n)
in col (succ (succ (succ 0))) end
