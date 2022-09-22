{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Other.Uniplate where

import Data.Data
import Data.Generics.Uniplate.Data
import Data.List (nub)
-- import Control.Monad.State (evalState)
import Control.Monad.State.Strict 

data Expr
  = Val Int
  | Add Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Mul Expr Expr
  | Neg Expr
  deriving (Show, Eq, Data, Typeable)



{- universe :: Uniplete on => on -> [on] -}

constants :: Expr -> [Int]
constants x = nub [y | Val y <- universe x]

checkDivByZero :: Expr -> Maybe Expr
checkDivByZero expr = 
  case take 1 [ err | err@(Div _ (Val 0)) <- universe expr] of
    [] -> Just expr
    _:_ -> Nothing 

  

{- transform :: Uniplate on => (on -> on) -> on -> on -}

optimize :: Expr -> Expr
optimize = transform f
  where 
    f (Neg (Val i)) = Val (negate i)
    f (Add x y) | x == y = Mul x (Val 2)
    f (Add x (Mul y (Val 2))) | x == y = Mul x (Val 3)
    f x = x

expr' =  Add (Div (Val 10) (Neg (Val 5))) (Mul (Div (Val 10) (Neg (Val 5))) (Val 2))

-- para :: Uniplate on => (on -> [res] -> res) -> on -> res


depth :: Expr -> Int 
depth expr = para (\_ cs -> 1 + maximum (0:cs)) expr

depthAdd :: Expr -> Int
depthAdd expr = maximum $ 0 : [depth e | e@(Add _ _) <- universe expr]

{- transformM :: (Monad m, Uniplate on) => (on -> m on) -> on -> m on -}


quickDepth :: Expr -> Int
quickDepth expr = execState (transformM go expr) 0
  where 
    go :: Expr -> State Int Expr
    go = undefined 

uniqueLits :: Expr -> Expr
uniqueLits expr = evalState (transformM f expr ) [10..]
  where
    f :: Expr -> State [Int] Expr
    f (Val _) = do
        ys <- get
        put (tail ys)
        pure (Val $ head ys)
    f x = pure x


uniqueLits' :: Expr -> Expr
uniqueLits' expr = evalState (transformM f expr ) (1,[])
  where
    f :: Expr -> State (Int,[Int]) Expr
    f (Val n) = do
        (y, ns) <- get
        if n `elem` ns
          then pure (Val 0)
          else do
            put (y + 1, n:ns)
            pure (Val y)
    f x = pure x


-- contexts :: Uniplate on => on -> [(on, on -> on)]

mutate :: Expr -> [Expr]
mutate x = concat [[gen $ Val $ i - 1, gen $ Val $ i+1] | (Val i, gen) <- contexts x]


mutate' :: Expr -> [Expr]
mutate' expr = expr:[ gen $ Add a a| (Mul a (Val 2), gen) <- contexts expr]

-- rewrite :: Uniplate on => (on -> Maybe on) -> on -> on

rew :: Expr -> Expr
rew expr = rewrite go expr

  where 
    go :: Expr -> Maybe Expr
    go (Val i) = if odd i then Just (Val $ i^2) else Nothing
    go x = Nothing


data Statement
    = Return Expression
    | If Expression Expression
    deriving(Data, Typeable, Show)

data Expression
    = Literal Int
    | Variable String
    | Binary Expression Int Expression
    deriving(Data, Typeable, Show)


ubi :: Statement -> [Expression]
ubi expr = [ n | n <- universeBi expr]

runUbi = ubi $ If (Variable "Hello") (Binary (Literal 5) 7 (Literal 12))