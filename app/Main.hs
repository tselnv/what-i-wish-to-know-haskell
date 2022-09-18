module Main (main) where

import Basics.Stacktraces qualified as BS

main :: IO ()
main = do
    print "Hello"
    !_ <- BS.main
    pure ()
