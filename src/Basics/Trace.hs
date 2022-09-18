module Basics.Trace where

import Debug.Trace
import Text.Printf
import Debug.Trace



example1 :: Int
example1 = trace "impure print" 1

example2 :: Int 
example2 = traceShow "tracing" 2

example3 :: [Int]
example3 = [trace "will no be called" 3]

main :: IO ()
main = do
    print example1
    print example2
    print $ length example3

traceM' :: (Monad m) => String -> m ()
traceM' string = trace string $ pure ()

traceShowM' :: (Show a , Monad m) => a -> m()
traceShowM' = traceM . show 

tracePrintfM' :: (Monad m, PrintfArg a) => String -> a -> m ()
tracePrintfM' s = traceM . printf s




