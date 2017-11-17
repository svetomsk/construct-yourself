{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tasks.GADT_1.GADTExpr where

data Lit :: * -> * where
  ILit :: Int  -> Lit Int
  BLit :: Bool -> Lit Bool

deriving instance Show a => Show (Lit a)
deriving instance Eq (Lit a)
deriving instance Ord (Lit a)

data Expr :: * -> * where
  Lit :: Lit a -> Expr a
  Add :: Expr Int  -> Expr Int  -> Expr Int
  Leq :: Expr Int  -> Expr Int  -> Expr Bool
  And :: Expr Bool -> Expr Bool -> Expr Bool

deriving instance Show a => Show (Expr a)
deriving instance Eq (Expr a)
deriving instance Ord (Expr a)

expr :: Expr a -> Lit a
expr (Lit x)   = x
expr (Add x y) = let ILit x' = expr x
                     ILit y' = expr y
                 in  ILit $ x' + y'
expr (Leq x y) = let ILit x' = expr x
                     ILit y' = expr y
                 in  BLit $ x' <= y'
expr (And x y) = let BLit x' = expr x
                     BLit y' = expr y
                 in  BLit $ x' && y'

i :: Int -> Expr Int
i = Lit . ILit

b :: Bool -> Expr Bool
b = Lit . BLit 

example1 :: Expr Bool -- 42 + 2 < 49 && True
example1 = ((i 42 `Add` i 2) `Leq` i 49) `And` b True

{-
Cannot be typed
example2 :: Expr a -- True + 5 < False
example2 = (b True `Add` i 5) `Leq` b False
-}
