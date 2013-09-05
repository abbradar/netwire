Netwire
=======

Netwire is a functional reactive programming (FRP) library with signal
inhibition.  It implements three related concepts, *wires*, *intervals*
and *events*, the most important of which is the *wire*.  To work with
wires we will need a few imports:

    import Control.Wire
    import Prelude hiding ((.), id)

The `Control.Wire` module exports the basic types and helper functions.
It also has some convenience reexports you will pretty much always need
when working with wires, including `Control.Category`.  This is why we
need the explicit `Prelude` import.

In general wires are generalized automaton arrows, so you can express
many design patterns using them.  The `netwire-frp` package provides a
proper FRP framework based on them, which strictly respects continuous
time and discrete event semantics.


Introduction
------------

The following type is central to the entire library:

    newtype Wire s e m a b

Don't worry about the large number of type arguments.  They all have
very simple meanings, which will be explained below.

A value of this type is called a *wire* and represents a *reactive*
value of type $b$, that is a value that may change over time.  It may
depend on a reactive value of type $a$.  In a sense a wire is a function
from a reactive value of type $a$ to a reactive value of type $b$, so
whenever you see something of type `Wire s e m a b` your mind should
draw an arrow from $a$ to $b$.  In FRP terminology a reactive value is
called a *behavior*.

A constant reactive value can be constructed using `pure`:

    pure 15

This wire is the reactive value 15.  It does not depend on other
reactive values and does not change over time.  This suggests that there
is an applicative interface to wires, which is indeed the case:

    liftA2 (+) (pure 15) (pure 17)

This reactive value is the sum of two reactive values, each of which is
just a constant, 15 and 17 respectively.  So this is the constant
reactive value 32.  Let's spell out its type:

    myWire :: (Monad m, Num b) => Wire s e m a b
    myWire = liftA2 (+) (pure 15) (pure 17)

This indicates that $m$ is some kind of underlying monad.  As an
application developer you don't have to concern yourself much about it.
Framework developers can use it to allow wires to access environment
values through a reader monad or to produce something (like a GUI)
through a writer monad.

The wires we have seen so far are rather boring.  Let's look at a more
interesting one:

    time :: (HasTime t s) => Wire s e m a t

This wire represents the current local time, which starts at zero when
execution begins.  It does not make any assumptions about the time type
other than that it is a numeric type with a `Real` instance.  This is
enforced implicitly by the `HasTime` constraint.

The type of this wire gives some insight into the $s$ parameter.  Wires
are generally pure and do not have access to the system clock or other
run-time information.  The timing information has to come from outside
and is passed to the wire through a value of type $s$, called the *state
delta*.  We will learn more about this in the next section about
executing wires.

Since there is an applicative interface you can also apply `fmap` to a
wire to apply a function to its value:

    fmap (2*) time

This reactive value is a clock that is twice as fast as the regular
local time clock.  If you use system time as your clock, then the time
type $t$ will most likely be `NominalDiffTime` from `Data.Time.Clock`.
However, you will usually want to have time of type `Double` or some
other floating point type.  There is a predefined wire for this:

    timeF :: (Fractional b, HasTime t s, Monad m) => Wire s e m a b
    timeF = fmap realToFrac time

If you think of reactive values as graphs with the horizontal axis
representing time, then the `time` wire is just a straight diagonal line
and constant wires (constructed by `pure`) are just horizontal lines.
You can use the applicative interface to perform arithmetic on them:

    liftA2 (\t c -> c - 2*t) time (pure 60)

This gives you a countdown clock that starts at 60 and runs twice as
fast as the regular clock.  So it after two seconds its value will be
56, decreasing by 2 each second.


Testing wires
-------------

Enough theory, we wanna see some performance now!  Let's write a simple
program to test a constant (`pure`) wire:

    import Control.Wire
    import Prelude hiding ((.), id)

    wire :: (Monad m) => Wire s () m a Integer
    wire = pure 15

    main :: IO ()
    main = testWire (pure ()) wire

This should just display the value 15.  Abort the program by pressing
Ctrl-C.  The `testWire` function is a convenience to examine wires.  It
just executes the wire and continuously prints its value to stdout:

    testWire ::
        (MonadIO m, Show b, Show e)
        => Session m s
        -> (forall a. Wire s e Identity a b)
        -> m c

The type signatures in Netwire are known to be scary. =) But like most
of the library the underlying meaning is actually very simple.
Conceptually the wire is run continuously step by step, at each step
increasing its local time slightly.  This process is traditionally
called *stepping*.

As an FRP developer you assume a continuous time model, so you don't
observe this stepping process from the point of view of your reactive
application, but it can be useful to know that wire execution is
actually a discrete process.

The first argument of `testWire` needs some explanation.  It is a recipe
for state deltas.  In the above example we have just used `pure ()`,
meaning that we don't use anything stateful from the outside world,
particularly we don't use a clock.  From the type signature it is also
clear that this sets `s = ()`.

The second argument is the wire to run.  The input type is quantified
meaning that it needs to be polymorphic in its input type.  In other
words it means that the wire does not depend on any other reactive
value.  The underlying monad is `Identity` with the obvious meaning that
this wire cannot have any monadic effects.

The following application just displays the number of seconds passed
since program start (with some subsecond precision):

    wire :: (HasTime t s) => Wire s () m a t
    wire = time

    main :: IO ()
    main = testWire clockSession_ wire

Since this time the wire actually needs a clock we use `clockSession_`
as the second argument:

    clockSession_ ::
        (Applicative m, MonadIO m)
        => Session m (Timed NominalDiffTime ())

It will instantiate $s$ to be `Timed NominalDiffTime ()`.  This type
indeed has a `HasTime` instance with $t$ being `NominalDiffTime`.  In
simpler words it provides a clock to the wire.  At first it may seem
weird to use `NominalDiffTime` instead of something like `UTCTime`, but
this is reasonable, because time is relative to the wire's start time.
Also later in the section about switching we will see that a wire does
not necessarily start when the program starts.


Constructing wires
------------------

Now that we know how to test wires we can start constructing more
complicated wires.  First of all it is handy that there are many
convenience instances, including `Num`.  Instead of `pure 15` we can
simply write `15`.  Also instead of

    liftA2 (+) time (pure 17)

we can simply write:

    time + 17

This clock starts at 17 instead of zero.  Let's make it run twice as
fast:

    2*time + 17

If you have trouble wrapping your head around such an expression it may
help to read `a*b + c` mathematically as $a(t) b(t) + c(t)$ and read
`time` as simply $t$.

So far we have seen wires that ignore their input.  The following wire,
found in the `netwire-frp` package, uses its input:

    integral 5

It literally integrates its input value with respect to time.  Its
argument is the integration constant, i.e. the start value.  To supply
an input simply compose it:

    integral 5 . 3

Remember that `3` really means `pure 3`, a constant wire.  The integral
of the constant 3 is $3 t + c$ and here $c = 5$.  Here is another
example:

    integral 5 . time

Since `time` denotes $t$ the integral will be $\frac{1}{2} t^2 + c$,
again with $c = 5$.  This may sound like a complicated, sophisticated
wire, but it's really not.  Surprisingly there is no crazy algebra or
complicated numerical algorithm going on under the hood.  Integrating
over time requires one addition and one division each frame.  So there
is nothing wrong with using it extensively to animate a scene or to move
objects in a game.

Sometimes categorical composition and the applicative interface can be
inconvenient, in which case you may choose to use the arrow interface.
The above integration can be expressed the following way:

    proc _ -> do
        t <- time -< ()
        integral 5 -< t

Since `time` ignores its input signal, we just give it a constant signal
with value `()`.  We name time's value $t$ and pass it as the input
signal to `integral`.


Events
------

Events are things that happen at certain points in time.  As such they
can be thought of as lists of values together with their occurrence
times:

    data Event a

The predefined `never` event is the event that never occurs:

    never :: Event a

As suggested by the type events contain a value.  Netwire exports the
constructors of the `Event` type, but `netwire-frp` doesn't.  This is
necessary in order to protect continuous time semantics.  You cannot
access event values directly.

We will need switching for that (covered in the next section).  Here is
an event that occurs right at the beginning:

    now :: Wire s e m a (Event a)

The event's value is the wire's input value at that point in time, in
this case at the beginning.  Example:

    now . time

This event's value will be 0, because time is zero at the beginning.
There are many wires for constructing events, but ultimately a framework
will provide the events you need.  For example a UI framework built on
Netwire will provide events for button clicks.  A game engine will
provide keyboard, mouse and joystick events.


Switching
---------

The main purpose of events is to switch.  Let's start with a very simple
example:  We have a wire that has the value "no":

    pure "no"

With the *OverloadedStrings* extension we can simply say:

    "no"

Now we want to make it switch to "yes" after three seconds.  First we
need an event that occurs at $t = 3$:

    at ::
        (HasTime t s, Monad m)
        => t
        -> Wire s e m a (Event a)

The `at 3` event occurs at $t = 3$.  For the actual switching we can use
the `switch` combinator:

    switch ::
        (Monad m, Monoid s)
        => Wire s e m a (b, Event (Wire s e m a b))
        -> Wire s e m a b

This wire acts like its argument wire until its event occurs, at which
point it switches to the wire contained in the event.  Now let's put all
of this together:

    no  = pure "no"
    yes = pure "yes"

These are constant wires and we want to switch from `no` to `yes` after
three seconds.  We need a switching event for that:

    switcher = at 3 . pure yes

This wire produces an event at $t = 3$.  The event's value will be `yes`
(the wire, not the string).  Finally let's put everything together:

    wire :: (HasTime t s, Monad m) => Wire s () m a String
    wire = switch (liftA2 (,) no switcher)

Such a switch may be easier to understand in arrow notation:

    wire =
        switch $ proc _ -> do
            ev <- at 3 -< pure "yes"
            id -< ("no", ev)

There are many kinds of switches.  Visit the Haddock documentations of
`Control.Wire.FRP.Switch` and `Control.Wire.FRP.Combine`.


Signal inhibition
-----------------

The final concept to learn is the concept of *signal inhibition* and
*occasions*.  As noted earlier Netwire allows signal inhibition, which
means that wires may choose not to have a value at all in certain time
intervals.  The difference between events and signal inhibition is that
events are discrete and instantaneous, while signal inhibition is
continuous.

Inhibiting wires are usually constructed from *occasions* which you may
think of as continuous events.  These are events which stretch over a
time period with non-zero length:

    type Occasion s e m a = Wire s e m a a
