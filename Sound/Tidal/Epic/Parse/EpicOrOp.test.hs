-- Interactive demonstration.
-- All of these tests work.

:s env/env.hs
import Data.Either (isRight)

f' = loope 0.2 can -- a 0.2-measure loop of the can sample
u' = fast 2        -- (u f) is a 0.1-measure loop of the can sample
b' = (+-)          -- concatenation
f = epicNotOp f'
b = binaryOp b'
u = unaryOp u'
(l,r) = (LeftBracket,RightBracket)

-- it works without brackets
stream = [f, b, u, f]
  -- = f `b` u f (because unary ops bind before binary ones)
  -- = a 0.3 second loop of two can samples, one twice as long as the other
Right g = parse parseEpicExpr "" stream
v1 g -- if v1 is a voice, then this will make that sound

-- alone, each bracketed term works
stream = [l,f,r]
isRight $ parse parseEpic "" stream

stream = [l,u,r]
isRight $ parse parseUnaryOp "" stream

stream = [l,b,r]
isRight $ parse parseBinaryOp "" stream

stream = [f,     l,b,r,     l,u,r,     l,f,r]
isRight $ parse parseEpicExpr "" stream

stream = [l,f,r,     l,b,r,     l,u,r,     l,f,r]
Right g = parse parseEpicExpr "" stream
v1 g
