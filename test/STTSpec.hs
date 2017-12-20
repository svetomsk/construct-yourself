{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction (Name, Term (..), Type (..), Context (..), Substitution (..)
  , Equation, Substitutable(substitute), compose, e, u)
import           Data.Map as Map (empty, fromList, unionWith)
import           Data.Set as Set (fromList, empty)
import           Test.Hspec
import Data.Set


main :: IO ()
main = hspec $ do
    describe "Substituion in context" testSubstitute
    describe "Substitution composing" testCompose
    describe "E algo test" testE

nameA = "a"
nameB = "b"
nameT = "t";
typeVarT = TVar "t"
typeVarB = TVar "b"
typeVarC = TVar "c"

emptyCtx = Context Map.empty

contextBT = Context ( Map.fromList[(nameB, typeVarT)])
contextBC = Context ( Map.fromList[(nameB, typeVarC)])
contextTB = Context ( Map.fromList[(nameT, typeVarB)])
contextTC = Context ( Map.fromList[(nameT, typeVarC)])

substAT = Substitution ( Map.fromList[(nameA, typeVarT)])
substBC = Substitution ( Map.fromList[(nameB, typeVarC)])
substTC = Substitution ( Map.fromList[(nameT, typeVarC)])


arrType = TArr typeVarT typeVarB

testSubstitute :: SpecWith ()
testSubstitute = do
    it "#1" $ substitute substAT emptyCtx `shouldBe` emptyCtx
    it "#2" $ substitute substAT contextBT `shouldBe` contextBT
    it "#3" $ substitute substBC contextTB `shouldBe` contextTC
    it "#4" $ substitute substAT typeVarT `shouldBe` typeVarT
    it "#5" $ substitute substBC typeVarB `shouldBe` typeVarC
    it "#6" $ substitute substBC arrType `shouldBe` (TArr typeVarT typeVarC)

testCompose :: SpecWith ()
testCompose = do
    it "#1" $ compose substAT substBC  `shouldBe` Substitution (Map.fromList[(nameB, typeVarC), (nameA, typeVarT)])
    it "#2" $ compose substTC substAT  `shouldBe` Substitution (Map.fromList[(nameA, typeVarC), (nameT, typeVarC)])

nameX = "x"
nameY = "y"
varX = Var nameX
varY = Var nameY
tpSigma = TVar "sigma"


ctxXY = Context (Map.fromList[(nameX, (TVar nameY))])
res1set = Set.fromList[(tpSigma, (TVar nameY))]
lamXY_X = (Lam nameX (Lam nameY varX))

res2set = Set.empty

testE :: SpecWith ()
testE = do
    it "#1" $ e ctxXY varX tpSigma `shouldBe` Just res1set
    it "#2" $ e emptyCtx lamXY_X tpSigma `shouldBe` Just res2set
