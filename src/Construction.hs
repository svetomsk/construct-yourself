{-# LANGUAGE OverloadedStrings #-}

module Construction
  ( Name, Term(..)
  , bound, free, fresh
  , reduce, substitute, alpha, beta, eta
  , termP, varP, appP, lamP, bracketP, betaNF, equal
  ) where

import           Construction.Internal.Functions (alpha, beta, bound, eta, free,
                                                  fresh, reduce, substitute, equal, hasRedex, betaNF, hasTermAsSubterm)
import           Construction.Internal.Parser    (appP, bracketP, lamP, termP,
                                                  varP)
import           Construction.Internal.Types     (Name, Term (..))

