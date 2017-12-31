-- One demonstration:
f a = a + 1              -- below I will (polymorphically) call f "+1"
u f = \a -> f a * 2
b f g = \a -> f a + g a  -- below I will (polymorphically) call b "+"
f' = funcNotOp' f
u' = unaryOp' u
b' = binaryOp' b
stream = [f', b', u', f']
  -- = f `b` u f (because unary ops bind before binary ones)
  -- = (+1) + u (+1)
  -- = (+1) + \a -> (a+1)*2
  -- = \a -> (a+1)*2 + (a+1)
  -- = \a -> 3a + 3
Right g = parse func "" stream

-- Equivalent expressions, using brackets
l = LeftBracket
r = RightBracket
stream = [l, f', r, b', u', f']
stream = [l, f', r, l, b', r, u', f']
stream = [l, f', r, l, b', r, l, u', r, f']
stream = [l, l, f', r, l, b', r, l, u', r, f', r]
stream = [l, l, f', r, l, l, b', r, r, l, u', r, f', r]
       -- (  (  f   )  (  (  b   )  )  (  u   )  f   )
stream = [ l, f', r,
           l, b', r,
           l, u', r,
           l, f', r]
-- the above all work

-- This works unless I allow the entire func parser to be bracketed.
-- In that case, it only parses the first line (the first f').
stream = [ l, l, f', r, r,
           l, b', r,
           l, u', r,
           l, f', r]

-- this parses as f = (+1) even though it's ill-formed (too many ls).
stream = [ l, l, f', r, r,
           l, l, b', r,
           l, u', r,
           l, f', r]

stream = [ l, l, f', r, r,
           l, l, b', r, r,
           l, l, u', r, r,
           l, l, f', r, r]
stream = [l, l, l,
                   l, l, f', r, r,
                   l, l, b', r, r,
                   l, l, u', r, r,
                   l, l, f', r, r,
           r, r, r]
