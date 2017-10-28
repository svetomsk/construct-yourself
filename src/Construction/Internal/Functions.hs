{-# LANGUAGE RecordWildCards #-}
                                 -- they make your code clean and clear.
                                 -- Read about this extension here:
                                 -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html



module Construction.Internal.Functions
  ( Context (..)        -- make restrictions is good practice. As you can see here,
  , fresh, free, bound  -- we make "public" not all functions, but only Context, fresh, ...
  , reduce, substitute, alpha, beta, eta
  )where

import           Construction.Internal.Types (Name, Term (..))
import           Data.Set                    (Set, delete, empty, insert,
                                              member, notMember, singleton,
                                              union)
import           Data.Text                   (pack)


-- Context is just set of names that are in our context.
type Context = Set Name

-- | @fresh@ generates new variable different from every variables in set.
fresh :: Set Name -> Name
fresh conflicts = head . dropWhile (`member` conflicts) $ nameGen -- This is ugly name generator. Make it better.
  where nameGen = [pack $ 'x' : show ind | ind <- [0..] :: [Int]]

-- | @free@ finds all free (Amazing!) variables from given term.
free :: Term -> Set Name
free (Var var)           = singleton var
free (App algo arg)      = free algo `union` free arg
free (Lam variable body) = variable `delete` free body

-- | @bound@ finds all bounded variables from given term.
-- This function uses RecordWildCards.
-- If you like it refactor @free@ function.
bound :: Term -> Set Name
bound Var{}   = empty
bound App{..} = bound algo `union` bound arg
bound Lam{..} = variable `insert` bound body

-- a[n := b] - substiturion
substitute :: Term -> Name -> Term -> Term
substitute v@Var{..} n b | var == n  = b
                         | otherwise = v
substitute a@App{..} n b = App (substitute algo n b) (substitute arg n b)
substitute l@Lam{..} n b | variable /= n && variable `member` (free b) == True = substitute (alpha l (free b)) n b
                         | variable == n = l
                         | otherwise = Lam variable (substitute body n b)

-- | alpha reduction
alpha :: Term -> Set Name -> Term
alpha a@(Lam v b@(Lam vv bb)) s = if v `member` s
                                  then let new = fresh s
                                       in Lam new (alpha (substitute b v (Var new)) s)
                                  else Lam v (alpha b s) 
alpha a@(Lam v b) s = if v `member` s
                      then let new = fresh s
                           in Lam new (substitute b v (Var new))
                      else a                 
alpha a s = a

-- | beta reduction
beta :: Term -> Term
beta (App (Lam var body) b) = beta (substitute body var b)
beta (App a b) = App (beta a) (beta b)
beta a = a

-- | eta reduction
eta :: Term -> Term
eta a@(Lam v (App m x)) = if v == (var x) && v `member` free m == False
                            then m
                            else a
eta a = a

-- | reduce term
reduce :: Term -> Term
reduce term = let term' = beta term
              in if term' == term
                 then let term'' = eta term
                      in if term'' == term
                         then term
                         else reduce term''
                 else reduce term'
