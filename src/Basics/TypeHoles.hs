module Basics.TypeHoles where

import GHC.Base hiding (Functor(..))

class MyFunctor f where
    fmap' :: (a -> b) -> f a -> f b 

--instance MyFunctor [] where
--    fmap' f (x:xs) = f x : fmap f _