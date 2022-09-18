module Basics.Stacktraces where


import Control.Exception

f x = g x

g x = error (show x)

main :: IO (Either SomeException ())
main = try (evaluate (f ()))