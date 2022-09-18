module Basics.Bottoms where


f :: a
f = let x = x in x

data ComplicatedType

f' :: a -> a
f' = undefined

data F = A | B


g :: F -> ()
g x = case x of
    A -> ()
    B -> error "<interactive>:3:11-31|case"


data Foo = Foo {example1 :: Int}

ff = Foo {}