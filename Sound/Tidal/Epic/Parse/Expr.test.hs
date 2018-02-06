-- Interactive demonstration.
-- All of these tests work.

:s env/env.hs
import Data.Either (isRight)

f' = loope 0.2 can -- a 0.2-measure loop of the can sample
g' = loope 0.2 cp  -- a 0.3-measure loop of the clap sample
u' = slow 2        -- (u f) is a 0.1-measure loop of the can sample
b' = (+-)          -- concatenation
f = epicNotOp f'
g = epicNotOp g'
b = binaryOp b'
u = unaryOp u'
(l,r) = (LeftBracket,RightBracket)

-- it works without brackets
stream = [g, g, b, u, f, f]
  -- = f `b` u f (because unary ops bind before binary ones)
  -- = a 0.3 second loop of two can samples, one twice as long as the other
Right x = parse parseEpicExpr "" stream
v1 x -- if v1 is a voice, then this will make that sound

-- alone, each bracketed term works
stream = [l,f,r]
isRight $ parse parseEpic "" stream

stream = [l,u,r]
isRight $ parse parseUnaryOp "" stream

stream = [l,b,r]
isRight $ parse parseBinaryOp "" stream

stream = [f,     l,b,r,     l,u,r,     l,f,r]
isRight $ parse parseEpicExpr "" stream

stream = [l,f,r,     l,b,r,     l,u,r,     l,f,r,    l,g,r]
Right x = parse parseEpicExpr "" stream
v1 x

stream = [l,f,r,   l,b,r,   l,u,r,   l,   l,f,r,    l,g,r,   r,   l,g,r,   g]
Right x = parse parseEpicExpr "" stream
v1 x
