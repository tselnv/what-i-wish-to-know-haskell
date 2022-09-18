-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Basics.Exhaustiveness where


boom = \(Just a) -> undefined 