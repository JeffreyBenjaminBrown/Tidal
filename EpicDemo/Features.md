## The Epic type

It's like a Pattern, but it includes a duration.

It offers a lawful Applicative instance. That is, if `e :: Epic a` and `f :: Epic (a -> b)`, then `f <*> e :: Epic b`. Also, there's a function `applyMetaEpic :: Epic (Epic a -> Epic b) -> Epic a -> Epic b`.


## Functions

# "Unary" operators: functions of type _ -> Epic -> Epic, maybe without the _

early :: Time -> Epic a -> Epic a
  This is what you use <~ to achieve outside of the Pattern parser.
  In the Epic parser, it is called "<".
late :: Time -> Epic a -> Epic a
  This is what you use ~> to achieve in the Pattern parser.
  In the Epic parser, it is called ">".
slow :: Time -> Epic a -> Epic a
  `slow t e` slows `e` by a factor of `t`.
  It changes the duration of `e`.
  In the Epic parser it is called /.
sparse :: Time -> Epic a -> Epic a
  `sparse t e` slows `e` by a factor of `t`,
  BUT does not change the duration of `e`.
  This is what you use / to achieve in the Pattern parser.
  In the Epic parser it is called //.
fast :: Time -> Epic a -> Epic a
  `fast t e` speeds up `e` by a factor of `t`.
  It changes the duration of `e`.
  In the Epic parser it is called *.
dense :: Time -> Epic a -> Epic a
  `dense t e` speeds up `e` by a factor of `t`,
  BUT does not change the duration of `e`.
  This is what you use * to achieve in the Pattern parser.
  In the Epic parser it is called **.
rev :: Epic a -> Epic a
  Just like the one for Patterns.


# Operators of type Epic -> Epic -> Epic

&* and &+ merge two Epics. They are what you use |*| and |+| to achieve outside the Pattern parser. 

+- concatenates two epics. If their durations differ, the result has a duration equal to their sum. 

+| plays two epics simultaneously. If their durations differ, that's okay.


## The parser

The parser is novel in a few ways, listed in this paragraph, detailed in the following sections. The parser offers a choice of parsing continuous or instantaneous events. The rhythm is not determined by any single Epic in a chain of merged Epics. It lets you mix different parameters in the same expression (e.g. gain, sample, speed). It lets parameter values stated at one point in the expression carry over into subsequent ones, or not. The binary operators that work outside of the parser work inside it, too, with the same syntax. Unary operators can also be applied in the parser, and they can be chained, without requiring brackets. Brackets (square ones) are available too. It parsers times as Rational values (written as numerator%denominator), but Double Values as Doubles (with a decimal point). Scale patterns can be parsed just like ordinary patterns.


# The parser offers a choice of parsing continuous or instantaneous events. 

There are two ways to parse a statement "x y". You might be saying "x happens for the first half, then y happens", or you might be saying "x happens at the start, then nothing until halfway through when y happens, and then nothing again until it repeats". The first method of parsing uses the command `pe`, and the second uses `pe0`.

That's if you're parsing a ParamEpic (i.e. an Epic (Map Param Value)). If you're parsing a scale, there are equivalent functions `ps` and `ps0`, although `ps0` seems silly -- scales generally apply over a length of time, not instantaneously.


# The rhythm is not determined by any single Epic in a chain of merged Epics.

In the Tidal Pattern parser, `sound "bd*8" |*| gain "1.1"` has eight notes in it, while `gain "1.1 |*| sound "bd*8"` has only one. That's because the rhythm is detemrined by the first Pattern.

Epics are simpler, in that the rhythm is determined by everything. The tradeoff, though, is that you have to make sure at least one of the Epics in a chain of merged Epics (joined with &+ or &*) is of the sort that renders to instantaneous values. 

For instance, `pe0 "_bd" &* pe "s1"` is good. That says "play the bd sample, at a speed of 1". So is `pe0 "_bd"` by itself. But `pe "_bd"` causes the bd sample to be playing all the time, rather than once. The result is a machine-gun effect, perhaps useful under certain conditions but probably not what you intended.


# It lets you mix different parameters in the same expression (e.g. gain, sample, speed). 

Space in the Epic parser, as in the Tidal parser, corresponds to concatenation. `s` stands for speed, `g` stands for gain, a leading `_` stands for "sample" (and a `_` by itself stands for silence), etc; to see a complete list, see Sound.Tidal.Epic.Parse.Params.

To indicate that a bunch of parameters happen at the same time, join them with two commas. So, for instance, `pe0 "_bd _sn,,g0.9"` says "first play the bass drum sample, then play the snare drum sample with a gain value of 0.9".


# The parser lets parameter values stated at one point in the expression carry over into subsequent ones, or not. 

So, for instance, `pe0 "_bd,,g0.9 _sn"` will play both samples at a gain value of 0.9, even though that values was only attached to the bass drum sample. To indicate that a paramter should not carry on, prefix it with the digit `1`: `pe0 "_bd,,1g0.9 _sn"` would play the nsare drum at the default gain value of 1.

# The binary operators that work outside of the parser work inside it, too

That is, &* (merge with multiplication), &+ (merge with addition), +- (concatenate) and +| (play simultaneously) are both ordinary Haskell operators and operators in the Epic parser.


# Unary operators can also be applied in the parser, and they can be chained, without requiring brackets. 

For instance, `pe0 "<1%8 *4 _bd _sn +- _hc", which uses two unary operators in front of a two-sample sequence, is valid.

# Brackets (square ones) are available too.

# It parsers times as Rational values (written as numerator%denominator), but Double Values as Doubles (with a decimal point).

In the Epic parser, durations for events can be given explicitly, with the `t` operator. For instance, "t1%8,,_cp t2,,_hc,,g0.9" will play a clap immediately, then (if cps = 1) will play the closed hat an eighth of a second later at a gain value of 0.9, and two seconds after that will repeat.


# Scale patterns can be parsed just like ordinary patterns.

Scales are a bit of a hack: They transform a meaningless "deg_p" (degree) parameter into a meaningful speed_p (speed) parameter. But the syntax is pretty clean:

  v1 $ ps "/3 maj loc" <*> pe0 "_psr,,d0 d2 d4"

plays a major scale for three seconds (if cps=1), then a locrian scale for three seconds. Over the first three seconds it plays the psr sample on scale degrees 0 (root), 2 and 4, and then it does that again. (Note that the scale pattern has a duration of 6, while the psr-degree pattern has a duration of 3.) The result is a major arpeggio alternating with a diminished one.