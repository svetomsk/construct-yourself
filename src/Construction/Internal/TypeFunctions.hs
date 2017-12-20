{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Construction.Internal.TypeFunctions where

import qualified Data.Map                        as M ((!), empty, lookup, unionWith, fromList, member, keysSet, insert, fromList, elems)
import           Data.Text                       (pack)
import           Data.Set                        as S (Set (..), elemAt, delete, singleton, toList, union, member, map, fromList)
import           Construction.Internal.Types
import           Data.Typeable                   (typeOf)
import           Construction.Internal.Functions hiding (Context, substitute)
import           Construction.Internal.Functions (fresh)

import           Data.Maybe                      
-- Split a set of elements to the first element and rest set
split :: Ord a => Set a -> (a, Set a)
split set = let x = elemAt 0 set
            in  (x, delete x set)

-- Take variable type from context or return Nothing
(!) :: Context -> Name -> Maybe Type
ctx ! x | M.member x (getCtx ctx) = Just $ getCtx ctx M.! x
        | otherwise             = Nothing

-- Something we can perform substitution with
class Substitutable a where
  substitute :: Substitution -> a -> a

--   Substitution in context
--   [a:=t]empty       => empty
--   [a:=t]{x:t1 ... } => {x:([a:=t]t1) ... }
instance Substitutable Context where
  substitute subst ctx =  let ctxValue = getCtx ctx
                    in if ctxValue == M.empty
                       then Context (M.empty)
                       else Context (fmap (\x -> (substitute subst x)) ctxValue)

-- Substitution in type:
--   [a:=t] a     => t
--   [a:=t] b     => b
--   [a:=t](r->p) => ([a:=t]r)->([a:=t]p)
instance Substitutable Type where
  substitute subst tp = case tp of
                      TVar{..} -> case (M.lookup tvar (getSubs subst)) of
                                    Just m -> m
                                    Nothing -> TVar tvar
                      TArr{..} -> TArr (substitute subst from) (substitute subst to)

-- Compose two substitutions
-- S@[a1 := t1, ...] . [b1 := s1 ...] = [b1 := S(s1) ... a1 := t1 ...]
compose :: Substitution -> Substitution -> Substitution
compose t s= Substitution (M.unionWith (\x y -> x) (fmap (\x -> substitute t x) (getSubs s)) (getSubs t) )

-- Create new context from free variables of some term
contextFromTerm :: Term -> Context
contextFromTerm term = Context $ M.fromList $ zip (toList $ free term) vars
  where
    vars = fmap (TVar . pack . ('a':) . show) [1..] 

-- Find all used types in given type
freeVars :: Type -> Set Type
freeVars a@TVar{..} = singleton a
freeVars TArr{..} = union (freeVars from) (freeVars to)

-- Return fresh TVar different from every in Context and given type
freshType :: Context -> Type -> Type
freshType ctx t = let ctxNames = S.map (\x-> (tvar x)) $ S.fromList(M.elems (getCtx ctx))
                  in case t of
                    TVar{..} -> TVar $ fresh (S.union ctxNames (singleton tvar))
                    TArr{..} -> let typeNames = S.map (\x -> tvar x) (S.union (freeVars from) (freeVars to))
                                in TVar $ fresh (S.union ctxNames typeNames)

-- Find a substitution that can solve the set of equations
u :: Set Equation -> Maybe Substitution
u set | null set  = pure mempty
      | otherwise = let (x, rest) = split set
                        a = fst x
                        b = snd x
                    in  if (typeOf a) == (typeOf b) && (typeOf b == typeOf (TVar "a")) 
                         then let subst = Substitution (M.fromList[(tvar a, b)])
                             in  if a == b                            then u rest
                                 else if S.member a (freeVars b)      then Nothing
                                 else if not (S.member a (freeVars b)) 
                                    then let us = u (S.map (\x -> (substitute subst (fst x), (substitute subst (snd x)))) rest)
                                          in case us of
                                              Just a -> Just (compose a subst)
                                              Nothing -> Nothing                                      
                                 else u (union (singleton (b, a)) rest)
                         else if (typeOf a) == (typeOf b) && (typeOf b == typeOf (TArr a b))
                         then let s1 = (from a)
                                  s2 = (to a)
                                  t1 = (from b)
                                  t2 = (to b)
                              in u (union (union (singleton (s1, t1)) (singleton (s2, t2))) rest)
                         else Nothing



maybeUnion :: Maybe (Set Equation) -> Maybe (Set Equation) -> Maybe (Set Equation)
maybeUnion s1 s2 = case s1 of
                    Just sa1 -> case s2 of
                              Just sa2 -> Just $ S.union sa1 sa2
                              Nothing  -> s1
                    Nothing  -> case s2 of
                              Just sa2 -> s2
                              Nothing -> Nothing

-- Generate equations set from some term
-- NB: you can use @fresh@ function to generate type names
e :: Context -> Term -> Type -> Maybe (Set Equation)
e ctx term tpe = case term of
                   Var{..} -> (\x -> singleton (tpe,x)) <$> ctx ! var
                   App{..} -> let alpha = freshType ctx tpe
                                  in maybeUnion (e ctx algo (TArr alpha tpe)) (e ctx arg  alpha)
                   Lam{..} -> let alpha = freshType ctx tpe
                                  beta = freshType ctx (TArr tpe alpha)
                                  firstE = e (Context (M.insert variable alpha (getCtx ctx))) body beta
                                  secondE = Just $ (singleton (tpe, (TArr alpha beta)))
                                  in maybeUnion firstE secondE

-- Find a principal pair of some term if exists
pp :: Term -> Maybe (Context, Type)
pp term = do let ctx = contextFromTerm term
             let tpe = TVar "r"
             eqs <- e ctx term tpe
             subs <- u eqs
             pure (substitute subs ctx, substitute subs tpe)
