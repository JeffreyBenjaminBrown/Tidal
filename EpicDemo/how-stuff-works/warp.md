`warp` lets you slow down a waveform by specifying how much you want to delay the 90 degree phase (the 25% mark). It leaves phase 0 and 180 unaffected, and arrives at phase 270 (the 75% mark) faster, symmetrically.

I'll say "the 25% mark", etc. for the 90 degree phase, etc. because it's more intuirive.

For instance, suppose we define the Epic `f` to be a unit-duration loop that plays a 1 at time 0, a 2 at time 1/4, a 3 at time 1/2, and a 4 at time 3/4:

    f = fast 4 $ cata 1 [1..4]

Let's define next a transformation `w`, which will warp time in the following manner: (1) It will leave unchanged what happens 0% and 50% of the way through the waveform (because that's where a sinewave is equal to 0). (2) For the first half of the waveform, it will slow things down. At "peak lag", the wave will be slowed down so much that what would have happened 25% of the way through instead happens (in this case) 30% of the way through. (3) In the second half of the wave it speeds up, symmetrically; what would have happened 75% of the way through the cycle instead happens 70% through.

We can define that function as follows:

    w = warp 0.001 0.30 1

The last argument is its period. The second to last indicates that we are slowing down the 25% mark to land on the 30% mark. The first is a tolerance, necessary because internally it converts floating points to rational numbers. (If I knew it was cpu-friendly I would just set that tolerance to a very low number.)

Here's how it looks in action. (plist is just a function that pretty-prints a list.)

    > plist $ arc f (0,1) -- the original (non-warped) Epic
    ((0 % 1,1 % 4),1)
    ((1 % 4,1 % 2),2)
    ((1 % 2,3 % 4),3)
    ((3 % 4,1 % 1),4)

    > plist $ arc (w f) (0,1) -- the warped Epic
    ((0 % 1,3 % 10),1)
    ((3 % 10,1 % 2),2)
    ((1 % 2,7 % 10),3)
    ((7 % 10,1 % 1),4)
