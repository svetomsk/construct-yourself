module Construction.Internal.Types
  ( Name, Term(..)
  , Type (..), Context (..), Substitution (..)
  , Equation, TypedTerm
  ) where

import Data.Text (Text) -- we want to import only Text from Data.Text.
import Data.Map  (Map (..))
import qualified Data.Map as Map
import Data.Set  (Set (..))


type Name = Text -- just alias, no more

data Term = Var { var :: Name }                     -- Variables: a, b, ...
          | App { algo :: Term, arg :: Term }       -- Application: M N
          | Lam { variable :: Name, body :: Term }  -- Abstraction: \x. M
  deriving (Show) -- we deriving some common classes like Show.
                  -- With this deriving you can use function "show"
                  -- to print your term.

data Type = TVar { tvar :: Name }                   -- Type variables: a, b, ...
          | TArr { from :: Type, to :: Type }       -- Arrow types: a -> b
  deriving (Eq, Ord, Show)

type TypedTerm = (Term, Type)

newtype Context = Context { getCtx :: Map Name Type } -- Types of variables
  deriving (Show)

newtype Substitution = Substitution { getSubs :: Map Name Type } -- Substitute type variable by some type
  deriving (Show)

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
