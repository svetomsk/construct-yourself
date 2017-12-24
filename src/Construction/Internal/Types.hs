{-# LANGUAGE RecordWildCards #-}

module Construction.Internal.Types
  ( Name, Term(..)
  , Type (..), Context (..), Substitution (..)
  , Equation
  ) where

import Data.Text (Text) -- we want to import only Text from Data.Text.
import Data.Map  (Map (..))
import qualified Data.Map as Map
import Data.Set  (Set (..))
import Data.List (intercalate)


type Name = Text -- just alias, no more

data Term = Var { var :: Name }                     -- Variables: a, b, ...
          | App { algo :: Term, arg :: Term }       -- Application: M N
          | Lam { variable :: Name, body :: Term }  -- Abstraction: \x. M

data Type = TVar { tvar :: Name }                   -- Type variables: a, b, ...
          | TArr { from :: Type, to :: Type }       -- Arrow types: a -> b
  deriving (Eq, Ord)

newtype Context = Context { getCtx :: Map Name Type } -- Types of variables

newtype Substitution = Substitution { getSubs :: Map Name Type } -- Substitute type variable by some type

type Equation = (Type, Type) -- Equation on types

mergeMaps :: Map Name Type -> Map Name Type -> Map Name Type
mergeMaps a b = Map.unionWith (\x y -> x) a b

instance Monoid Context where
  mempty = Context Map.empty
  mappend a b = Context (mergeMaps (getCtx a) (getCtx b))

instance Eq Context where
  a == b = (getCtx a) == (getCtx b)

instance Monoid Substitution where
  mempty = Substitution Map.empty
  mappend a b = Substitution (mergeMaps (getSubs a) (getSubs b))

instance Eq Substitution where
  a == b = (getSubs a) == (getSubs b)

joinList :: [String] -> String
joinList list = intercalate "," list

mapToList :: Map Name Type -> [String]
mapToList mp = (\x -> (show $ fst x) ++ ":" ++ (show $ snd x)) <$> (Map.toList mp)

instance Show Context where
  show x = "ctx: { " ++ (joinList $ mapToList $ getCtx x) ++ " }"

instance Show Substitution where
  show x = "subst: { " ++ (joinList $ mapToList $ getSubs x) ++ " }"

instance Show Type where
  show x = case x of
            TVar {..} -> show tvar
            TArr {..} -> "(" ++ (show from) ++ "->" ++ (show to) ++ ")"

instance Show Term where
  show t = case t of
            Var{..} -> show var
            App{..} -> "(" ++ (show algo) ++ " " ++ (show arg) ++ ")"
            Lam{..} -> "\\" ++ (show variable) ++ ".(" ++ (show body) ++ ")"
