{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction (Name, Term (..), Type (..), Context (..), Substitution (..)
  , Equation, Substitutable(substitute), compose, e, u)
import           Data.Map as Map (empty, fromList, unionWith, (!))
import           Data.Set as Set (fromList, empty)
import           Test.Hspec
import           Data.Set
import           Data.Maybe as MB (fromJust)

main :: IO ()
main = hspec $ do
    describe "Substituion in context" testSubstitute
    describe "Substitution composing" testCompose
    describe "E algo test" testE
    describe "U algo test" testU

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

nameX0 = "x0"
nameX1 = "x1"
nameX2 = "x2"
nameX3 = "x3"
nameX = "x"
nameY = "y"
nameSigma = "sigma"

nameAplha = "alpha"
nameBeta = "beta"

varX = Var nameX
varY = Var nameY


tpSigma = TVar nameSigma
tpX = TVar nameX
tpY = TVar nameY
tpX0 = TVar nameX0
tpX1 = TVar nameX1
tpX2 = TVar nameX2
tpX3 = TVar nameX3
tpAlpha = TVar nameAplha
tpBeta = TVar nameBeta


ctxXY = Context (Map.fromList[(nameX, tpY)])
res1set = Set.fromList[(tpSigma, tpY)]
lamXY_X = Lam nameX $ Lam nameY varX

res2set = Set.fromList([(tpSigma, (TArr tpX0 tpX1)), (tpX1, (TArr tpX2 tpX3)), (tpX3, tpX0)])

appXY = App varX varY
ctxXaYb = Context (Map.fromList[(nameX, TArr tpAlpha tpBeta), (nameY, tpAlpha)])
res3set = Set.fromList([(tpX0, tpAlpha), (TArr tpX0 tpSigma, TArr tpAlpha tpBeta)])


testE :: SpecWith ()
testE = do
    it "#1" $ e ctxXY varX tpSigma `shouldBe` Just res1set
    it "#2" $ e emptyCtx lamXY_X tpSigma `shouldBe` Just res2set
    it "#3" $ e ctxXaYb appXY tpSigma `shouldBe`Just res3set
    it "#4" $ e emptyCtx appXY tpSigma `shouldBe` Nothing

tpX2X0 = TArr tpX2 tpX0

tpX020 = TArr tpX0 tpX2X0
tpFail = TVar "fail"
noType = TVar "notype"


unpackSubstituion :: Maybe (Set Equation) -> Type

unpackSubstituion set = case set of
                            Nothing -> noType
                            Just a -> let solution = u $ MB.fromJust set
                                        in case solution of
                                            Just s -> (getSubs s) Map.! nameSigma
                                            Nothing -> tpFail

testU :: SpecWith ()
testU = do
    it "#1" $ unpackSubstituion (e ctxXY varX tpSigma) `shouldBe` tpY
    it "#2" $ unpackSubstituion (e emptyCtx lamXY_X tpSigma) `shouldBe` tpX020
    it "#3" $ unpackSubstituion (e ctxXaYb appXY tpSigma) `shouldBe` tpBeta
    it "#4" $ unpackSubstituion (e emptyCtx appXY tpSigma) `shouldBe` noType