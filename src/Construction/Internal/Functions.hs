 {-# LANGUAGE RecordWildCards #-}
                                 -- they make your code clean and clear.
                                 -- Read about this extension here:
                                 -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html



module Construction.Internal.Functions
  ( Context (..)        -- make restrictions is good practice. As you can see here
  , fresh, free, bound  -- we make "public" not all functions, but only Context, fresh, ...
  , reduce, substitute, alpha, beta, eta, equal, hasRedex, betaNF, hasTermAsSubterm
  ) where

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
bound (Var a)             = empty
bound (App algo arg)      = bound algo `union` bound arg
bound (Lam variable body) = variable `insert` bound body

-- a[n := b] - substiturion
substitute :: Term -> Name -> Term -> Term
substitute v@(Var var) n b | var == n  = b
                           | otherwise = v
substitute a@(App algo arg) n b = App (substitute algo n b) (substitute arg n b)
substitute l@(Lam variable body) n b | variable /= n && variable `member` (free b) == True = substitute (alpha l (free b)) n b
                                     | variable == n = l
                                     | otherwise = Lam variable (substitute body n b)

alpha :: Term -> Set Name -> Term

alpha Lam{..} conflicts | hasConflict = let all_conflicts = conflicts `union` free body
                                            n_variable = fresh all_conflicts
                                            n_body = substitute body variable (Var n_variable)
                                        in Lam n_variable n_body
                        | otherwise   = Lam variable (alpha body conflicts)
                      where hasConflict = variable `member` conflicts
alpha App{..} conflicts = App (alpha algo conflicts) (alpha arg conflicts)
alpha var _ = var

beta :: Term -> Term

beta (App (Lam n e1) e2) = substitute e1 n e2
beta App{..} = let b_algo = beta algo
               in if b_algo /= algo then App b_algo arg else App algo (beta arg)
beta (Lam n e) = Lam n (beta e)
beta var = var

eta :: Term -> Term

eta l@(Lam v (App algo (Var e))) | hasEta    = algo
                                 | otherwise = l
  where hasEta = v == e && v `notMember` free algo
eta term = term


instance Eq Term where
  Var v1 == Var v2 = v1 == v2
  App algo1 arg1 == App algo2 arg2 = algo1 == algo2 && arg1 == arg2
  Lam v1 b1 == Lam v2 b2 = sub1 == sub2
    where
      freshVar = Var $ fresh $ free b1 `union` free b2
      sub1 = substitute b1 v1 freshVar
      sub2 = substitute b2 v2 freshVar
  _ == _ = False

-- | reduce term
reduce :: Term -> Term
reduce term = let term' = beta term
              in if term' == term
                 then let term'' = eta term
                      in if term'' == term
                         then term
                         else reduce term''
                 else reduce term'


equal :: Term -> Term -> Bool
equal a b = postequal (reduce a) (reduce b)

postequal :: Term -> Term -> Bool
postequal (Var v1) (Var v2) = v1 == v2
postequal a1@(App a b) a2@(App c d)               = (equal a c) && (equal b d)
postequal l1@(Lam x1 body1) l2@(Lam x2 body2)     = if x1 /= x2
                                                    then equal l1 (Lam x1 (substitute body2 x2 (Var x1)))
                                                    else equal body1 body2
postequal _ _ = False

hasRedex :: Term -> Bool
hasRedex (App (Lam x b) c) = True
hasRedex (App a b) = (hasRedex a) || (hasRedex b)
hasRedex a = False

hasTermAsSubterm :: Term -> Term -> Bool
hasTermAsSubterm app@(App a b) c = (app == c) || hasTermAsSubterm a c || hasTermAsSubterm b c
hasTermAsSubterm lam@(Lam x b) c = (lam == c) || hasTermAsSubterm b c
hasTermAsSubterm vv@(Var v) c    = (vv == c)


betaNF :: Term -> Bool
betaNF t = betaNFhelp t t

betaNFhelp :: Term -> Term -> Bool
betaNFhelp s t = let t' = beta t
           in if t' == t
              then if (hasRedex t')
                   then False
                   else True
              else if (hasTermAsSubterm t' s) 
                   then False
                   else betaNFhelp s t'
