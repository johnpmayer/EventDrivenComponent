
> {-# OPTIONS -Wall -Werror #-}
> module Control.Reactive.Component where
> import Control.Monad.State (State,get,put,runState)
> import Data.Maybe ()

Component:

I: a set of input event variables
O: a set of output event variables
S: a set of state variables
T: a set of tasks t : that read from I+S and write to S+O

An event variable is typed as follows

> data Event a = NoChange | Update a
>   deriving (Eq, Ord, Show)

Here's our definition of a Component. Recall that types are sets, 
so we parameterize the type by I, O, and S

> data Component i s o = Component {
>   initialState :: s,
>   computeBoth :: (i, s) -> (s, o) }

Let's try an example. 

> count :: Component String Int String
> count = Component 0 countStep where
>   countStep (i,s) = (s+1, show s ++ ":" ++ i)

> cat :: Component String String String
> cat = Component "" concatStep where
>   concatStep (i,s) = (s++i,s++i)

> step :: Eq o => Component i s o -> i -> State (Maybe (s,o)) (Event o)
> step c i =
>   do
>     msave <- get
>     let (s',o') = case msave of
>                     Nothing    -> computeBoth c (i, initialState c)
>                     Just (s,_) -> computeBoth c (i, s)
>     put $ Just (s',o')
>     return $ case msave of
>       Nothing -> Update o'
>       Just (_,o)  -> if o == o' then NoChange else Update o'

Here it is in user land

> test1 :: IO ()
> test1 = let component = count
>             loop saved = do
>               input <- getLine
>               let (out, saved') = runState (step component input) saved
>               putStrLn $ show out
>               loop saved'
>         in loop Nothing >> return ()

> test2 :: IO ()
> test2 = let component = cat
>             loop saved = do
>               input <- getLine
>               let (out, saved') = runState (step component input) saved
>               putStrLn $ show out
>               loop saved'
>         in loop Nothing >> return ()

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

> data LatchedComponent i o s = LatchedComponent {
>   initialLatchedState :: s,
>   computeOutput :: s -> Event o,
>   computeState :: Event i -> s }


