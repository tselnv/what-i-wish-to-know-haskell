module Basics.Haddok (
    -- * My Header
    example1,
    example2,
    fmap',
    f,
    ) where

import Basics.Trace (example1)

-- | Here we use the "Data.Text" library and import
-- the 'Data.Text.pack' function.
-- | An example of a code block.
--
-- @
-- f x = f (f x)
-- @
-- > f x = f (f x)
-- | Example of an interactive shell session.
--
-- >>> factorial 5
-- 120

-- | Documentation for f
f :: a -> a
f = id

-- | Multiline documentation for the function
-- f with multiple arguments.
fmap' :: Functor f 
  => (a -> b) -- ^function
  -> f a -- ^input
  -> f b -- ^output
fmap' = undefined 

data T a b
  = A a -- ^Documentation for 'A'
  | B b -- ^Documentation for 'B'


example2 = undefined 
