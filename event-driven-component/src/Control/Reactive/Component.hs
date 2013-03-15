
{-# OPTIONS -Wall -Werror #-}
module Control.Reactive.Component where
import Control.Monad.State (State,get,put,runState,execState)
import Data.Maybe ()

{-

Component:

I: a set of input event variables
O: a set of output event variables
S: a set of state variables
T: a set of tasks t : that read from I+S and write to S+O

An event variable is typed as follows

-}

data Event a = NoChange | Update a
  deriving (Eq, Ord, Show)

{-

Here's our definition of a Component. Recall that types are sets, 
so we parameterize the type by I, O, and S

-}

data Component i s o = C { step :: i -> State s o }

-- Let's try an example. 

count :: Component String Int String
count = C step' where
  step' input = 
    do
      n <- get
      put (n+1)
      return $ show n ++ ":" ++ input

testC :: Component String a String -> a -> IO ()
testC component initial =
  do
    putStrLn "Round Start"
    input <- getLine
    let (output, next) = runState (step component $ input) initial
    putStrLn output
    testC component next

{-

We have a notion where a variable awaits another variable.
The variable y awaits x if there is some task that reads x and writes y

It is possible that none, some, or all of the variables in O
await some variables in I.

A simplification we can use is to only consider two classes
of components: those where no member of O awaits a member of I
variables, and everything else, where some subset of O awaits
some subset of I.

The first case is special; we can use the knowledge that O
does not await I to compose components in a way that looks like
a loop, but will not have a cycle of await dependencies.

-}

data LatchedComponent i s o = LC {
  latch :: s -> o,
  update :: i -> State s () }

delay :: LatchedComponent String String String
delay = LC latch' update' where
  latch' = id
  update' = put
  
testLC :: LatchedComponent String a String -> a -> IO ()
testLC component initial =
  do
    putStrLn "Round Start"
    putStrLn . latch component $ initial
    input <- getLine
    let next = execState (update component $ input) initial
    testLC component next