I have been thinking about what it means to run a 'Producer'
twice. Specifically -- whether the Producer resumes where it left of
or not. I think that in general the behavior is undefined. I feel like
this has not been explicitly stated much -- so I am going to say it
now.

Consider two different cases:

 1. a producer that produces values from a pure list

 2. a producer that produces values from a network connection


If we run the first producer twice we will get the same answer each
time. If we run the second producer twice -- we will likely get
different results -- depending on what data is available from the
network stream.

Now -- that is not entirely surprising -- one value is pure and one is
based on IO. So that is no different than calling a normal pure
function versus a normal IO function.

But -- I think it can be easy to forget that when writing pipes
code. Imagine we write some pipes code that processes a network stream
-- and it relies on the fact that the network Producer automatically
resumes from where it left off.

Now, let's pretend we want to test our code. So we create a pure
Producer that produces the same bytestring that the network pipe was
producing. Alas, our code will not work because the pure Producer does
not automatically resume when called multiple times.

I think this means that we must assume, by default, that the Producer
does not have resumable behavior. If we want to write code that relies
on the resumable behavior -- then we must explictly ensure that it
happens.

In pipes-parse the resumability is handled by storing the 'Producer'
in 'StateT'.

Another alternative is to use an 'IORef'. I have an example of the
'IORef' solution below.

> module Main where

> import Data.IORef             (IORef(..), newIORef, readIORef, writeIORef)
> import           Pipes
> import qualified Pipes.Prelude as P

Here is our pure Producer:

> pure10 :: (Monad m) => Producer Int m ()
> pure10 = mapM_ yield [1..10]

And here is a function which uses a Producer twice.

> take5_twice :: Show a => Producer a IO () -> IO ()
> take5_twice p =
>     do runEffect $ p >-> P.take 5 >-> P.print
>        putStrLn "<<Intermission>>"
>        runEffect $ p >-> P.take 5 >-> P.print

Note that we have limited ability reason about the results since we do
not know if the 'Producer' is resumable or not.

If we run 'take5_twice' using our pure Producer:

> pure10_test :: IO ()
> pure10_test =
>     take5_twice pure10

it will restart from 1 each time:

    > pure10_test
    1
    2
    3
    4
    5
    <<Intermission>>
    1
    2
    3
    4
    5

Here is a (not very generalized) function that uses an 'IORef' to
store the current position in the 'Producer' -- similar to how
'StateT' works:

> resumable :: Producer Int IO () -> IO (Producer Int IO ())
> resumable p0 =
>    do ref <- liftIO $ newIORef p0
>       return (go ref)
>    where
>      go :: IORef (Producer Int IO ()) -> Producer Int IO ()
>      go ref =
>          do p <- liftIO $ readIORef ref
>             x <- liftIO $ next p
>             case x of
>               (Right (i, p')) ->
>                   do liftIO $ writeIORef ref p'
>                      yield i
>                      go ref
>               (Left ()) ->
>                   do liftIO $ writeIORef ref (return ())
>                      return ()

Now if we call 'take5_twice' with our resumable Producer:

> impure10_test :: IO ()
> impure10_test =
>     do p <- resumable pure10
>        take5_twice p

Here we see the resuming behavior:

    > impure10_test
    1
    2
    3
    4
    5
    <<Intermission>>
    6
    7
    8
    9
    10

If we call 'resumable' on a 'Producer' that already has resumable
behavior -- it will still work. We can simulate that by calling resumable twice:

> twice_resumable :: IO ()
> twice_resumable =
>     do p0 <- resumable pure10
>        p  <- resumable p0
>        take5_twice p


    > twice_resumable
    1
    2
    3
    4
    5
    <<Intermission>>
    6
    7
    8
    9
    10

Of course, we now have the overhead of *two* 'IORef' based Producers.

So we are now left with some questions of style.

If we are writing something like an HTTP server -- we can assume that
most of the time we are going to working with a 'Producer' based on a
resumable source like a network stream. So, by using the inherent
resumability we can presumably get lower overhead and higher
performance. If we need to use the code with a non-resumable Producer
then we can use a function like 'resumable' to fake it.

This is somewhat distasteful in two ways though.

 (1) It forces everything to be in the IO monad -- even when
     everything could actually be pure.

 (2) it relies on the resumability of the Producer -- but there is no
     enforcement or indication of that in the type system.


The alternative is to run all our code inside a 'StateT'. Since the
'StateT' takes care of resuming we do not have to worry if the
underlying Producer does or not. But.. now we always have the overhead
of being inside a 'StateT' even we don't really need to be -- so we
have a more complicated set of types to work with and more potential
overhead.

The upside is that our pure code stays pure. We only introduce the IO
monad when IO is really used.

This is the major decision blocking hyperdrive at the
moment. (hyperdrive is my pipes based HTTP server).

Any thoughts?

- jeremy

