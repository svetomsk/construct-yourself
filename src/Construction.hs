{-# LANGUAGE OverloadedStrings #-}

module Construction
  ( Name, Term(..), Type (..), Context (..), Substitution (..)
  , Equation, Substitutable(substitute), TypedTerm
  , bound, free, fresh
  , reduce, alpha, beta, eta
  , termP, varP, appP, lamP, typeP, bracketP, betaNF, equal, compose, e, u
  ) where

import           Construction.Internal.Functions (alpha, beta, bound, eta, free,
                                                  fresh, reduce, substitute, equal, hasRedex, betaNF, hasTermAsSubterm)
import           Construction.Internal.Parser    (appP, bracketP, lamP, termP,
                                                  varP, typeP)
import           Construction.Internal.Types     (Name, Term (..), Type (..), Context (..), Substitution (..), TypedTerm
  , Equation)

import           Construction.Internal.TypeFunctions (Substitutable(..), compose, u, e)
