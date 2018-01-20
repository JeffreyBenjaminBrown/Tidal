Nested transformations on rhythms are something that's particularly easy in Sound.Tidal.Epic.

Here's how we would create a duration-4 pattern that goes 1,2,3,4 over the course of 4 seconds. (I'm assuming cps = 1.)

> w = cata 1 [1..4]

"_arc ep (s,e)" shows what the Epic ep does between times s and e. We can use _arc to verify that w goes 1,2,3,4:

> plist $ _arc w (0,8)  -- _arc rather than arc because now Epic uses lenses
((0 % 1,1 % 1),1)
((1 % 1,2 % 1),2)
((2 % 1,3 % 1),3)
((3 % 1,4 % 1),4)
((4 % 1,5 % 1),1)
((5 % 1,6 % 1),2)
((6 % 1,7 % 1),3)
((7 % 1,8 % 1),4)

Now suppose we wanted to divide that infinite sequence into 3-second chunks, and insert one second of 0 between each chunk. First let's define x to be the Epic divided into 3-second chunks:

> x = period .~ Just 3 $ w

All I've done there is use .~ (a lens operator) to set the period of w to 3 instead of 4. (I haven't actually changed w; I've just made x equal to what you'd get if you did that to w.)

The way we would create a 1-second loop of 0 is with "loopa 1 0". (It's called loopa because it creates an "Epic a", for any type "a".) So if we x to that one-second loop of 0 using (+-), we get what we wanted:

> plist $ _arc (x +- loopa 1 0) (0,8)
((0 % 1,1 % 1),1) -- here begin three seconds of 1,2,3,4 starts here
((1 % 1,2 % 1),2)
((2 % 1,3 % 1),3)
((3 % 1,4 % 1),0) -- here's the 0 stuck between each 3-second chunk of 1,2,3,4
((4 % 1,5 % 1),4) -- here begin another three seconds of 1,2,3,4
((5 % 1,6 % 1),1)
((6 % 1,7 % 1),2)
((7 % 1,8 % 1),0) -- here's the next intervening zero

Now suppose we want to add 10 to every third element of that infinite 1,2,3,4 sequence. Let's define a pattern of those operations:

> ops = cata 1 [(+10),id,id]

And define y to be what you get when you apply those operations to x:

> y = ops <*> x

We can check that it works:

> plist $ _arc (y +- loopa 1 0) (0,12)
((0 % 1,1 % 1),11)
((1 % 1,2 % 1),2)
((2 % 1,3 % 1),3)
((3 % 1,4 % 1),0)
((4 % 1,5 % 1),14)
((5 % 1,6 % 1),1)
((6 % 1,7 % 1),2)
((7 % 1,8 % 1),0)
((8 % 1,9 % 1),13)
((9 % 1,10 % 1),4)
((10 % 1,11 % 1),1)
((11 % 1,12 % 1),0)
