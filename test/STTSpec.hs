{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction (Name, Term (..), Type (..), Context (..), Substitution (..)
  , Equation, Substitutable(substitute), compose, e, u, termP)
import           Data.Map as Map (empty, fromList, unionWith, (!))
import           Data.Set as Set (fromList, empty)
import           Test.Hspec
import           Data.Set
import           Data.Maybe as MB (fromJust)
import           Text.Parsec
import           Text.Parsec.Text

main :: IO ()
main = hspec $ do
    describe "Test context monoid" testContextMonoid
    describe "Substituion in context" testSubstInContext
    describe "Substitution in type" testSubstInType
    describe "Substitution composing" testCompose
    describe "E algo test" testE
    describe "U algo test" testU

mkCtx :: [(Name, Type)] -> Context
mkCtx list = Context $ Map.fromList $ list

mkSubst :: [(Name, Type)] -> Substitution
mkSubst list = Substitution $ Map.fromList $ list

--parseTerm :: 

nameA = "a"
nameB = "b"
nameT = "t";
nameX0 = "x0"
nameX1 = "x1"
nameX2 = "x2"
nameX3 = "x3"
nameX = "x"
nameY = "y"
nameSigma = "sigma"
nameAplha = "alpha"
nameBeta = "beta"

typeVarT = TVar "t"
typeVarB = TVar "b"
typeVarC = TVar "c"
tpSigma = TVar nameSigma
tpX = TVar nameX
tpY = TVar nameY
tpX0 = TVar nameX0
tpX1 = TVar nameX1
tpX2 = TVar nameX2
tpX3 = TVar nameX3
tpAlpha = TVar nameAplha
tpBeta = TVar nameBeta
arrType = TArr typeVarT typeVarB
tpX2X0 = TArr tpX2 tpX0
tpX020 = TArr tpX0 tpX2X0
tpFail = TVar "fail"
noType = TVar "notype"

emptyCtx = Context Map.empty

contextBT = mkCtx [(nameB, typeVarT)]
contextBC = mkCtx [(nameB, typeVarC)]
contextTB = mkCtx [(nameT, typeVarB)]
contextTC = mkCtx [(nameT, typeVarC)]
ctxXaYb = mkCtx [(nameX, TArr tpAlpha tpBeta), (nameY, tpAlpha)]
ctxXY = mkCtx [(nameX, tpY)]

emptySubst = Substitution (Map.empty)
substAT = mkSubst [(nameA, typeVarT)]
substBC = mkSubst [(nameB, typeVarC)]
substTC = mkSubst [(nameT, typeVarC)]
substAC = mkSubst [(nameA, typeVarC)]
substXX0 = mkSubst [(nameX, tpX0)]
substX01 = mkSubst [(nameX0, tpX1)]
substX12 = mkSubst [(nameX1, tpX2)]
substXA01 = mkSubst [(nameX, TArr tpX0 tpX1)]
substX1A23 = mkSubst [(nameX1, TArr tpX2 tpX3)]
substX30 = mkSubst [(nameX3, tpX0)]


varX = Var nameX
varY = Var nameY
lamXY_X = Lam nameX $ Lam nameY varX
appXY = App varX varY
lamXX = Lam nameX varX
appLam = App (Lam "x" varX) varY

res1set = Set.fromList [(tpSigma, tpY)]
res2set = Set.fromList([(tpSigma, (TArr tpX0 tpX1)), (tpX1, (TArr tpX2 tpX3)), (tpX3, tpX0)])
res3set = Set.fromList([(tpX0, tpAlpha), (TArr tpX0 tpSigma, TArr tpAlpha tpBeta)])
res4set = Set.fromList([(tpX1, tpX0), (tpSigma, TArr tpX0 tpX1)])

testContextMonoid :: SpecWith () 
testContextMonoid = do
    it "#1" $ mappend mempty contextBT `shouldBe` contextBT
    it "#2" $ mappend contextBC mempty `shouldBe` contextBC
    it "#3" $ mappend contextBC contextTB `shouldBe` Context (Map.fromList [(nameB, typeVarC), (nameT, typeVarB)])
    it "#4" $ mappend mempty mempty `shouldBe` emptyCtx
    it "#5" $ mappend contextBT contextBC `shouldBe` contextBT

testSubstitutionMonoid :: SpecWith ()
testSubstitutionMonoid = do
    it "#1" $ mappend mempty substAT `shouldBe` substAT
    it "#2" $ mappend substAT mempty `shouldBe` substAT
    it "#3" $ mappend substAT substBC `shouldBe` mkSubst [(nameA, TVar "t"), (nameB, typeVarC)]
    it "#4" $ mappend mempty mempty `shouldBe` Substitution Map.empty
    it "#5" $ mappend substAT substAC `shouldBe` substAT

testSubstInContext :: SpecWith ()
testSubstInContext = do
    it "#1" $ substitute substAT emptyCtx `shouldBe` emptyCtx
    it "#2" $ substitute substAT contextBT `shouldBe` contextBT
    it "#3" $ substitute substBC contextTB `shouldBe` contextTC
    it "#4" $ substitute substBC ( substitute substAT contextTB) `shouldBe` contextTC
    it "#5" $ substitute substAT ( substitute substBC contextTB) `shouldBe` contextTC

testSubstInType :: SpecWith ()
testSubstInType = do
    it "#1" $ substitute substAT typeVarT `shouldBe` typeVarT
    it "#2" $ substitute substBC typeVarB `shouldBe` typeVarC
    it "#3" $ substitute substBC arrType `shouldBe` (TArr typeVarT typeVarC)
    it "#4" $ substitute substBC (TArr arrType typeVarB) `shouldBe` (TArr (TArr typeVarT typeVarC) typeVarC)
    it "#5" $ substitute substBC (TArr arrType (TArr typeVarC typeVarB)) `shouldBe` (TArr (TArr typeVarT typeVarC) (TArr typeVarC typeVarC))

testCompose :: SpecWith ()
testCompose = do
    it "#1" $ compose emptySubst substAT `shouldBe` substAT
    it "#2" $ compose substAT substBC  `shouldBe` Substitution (Map.fromList[(nameB, typeVarC), (nameA, typeVarT)])
    it "#3" $ compose substTC substAT  `shouldBe` Substitution (Map.fromList[(nameA, typeVarC), (nameT, typeVarC)])
    it "#4" $ compose substX12 (compose substX01 substXX0) `shouldBe` Substitution (Map.fromList [(nameX, tpX2), (nameX0, tpX2), (nameX1, tpX2)])
    it "#5" $ compose substX30 (compose substX1A23 substXA01) `shouldBe` Substitution (Map.fromList [(nameX3, tpX0), (nameX1, TArr tpX2 tpX0), (nameX, TArr tpX0 (TArr tpX2 tpX0))])

testE :: SpecWith ()
testE = do
    it "#1" $ e ctxXY varX tpSigma `shouldBe` Just res1set
    it "#2" $ e emptyCtx lamXY_X tpSigma `shouldBe` Just res2set
    it "#3" $ e ctxXaYb appXY tpSigma `shouldBe`Just res3set
    it "#4" $ e emptyCtx appXY tpSigma `shouldBe` Nothing
    it "#5" $ e emptyCtx lamXX tpSigma `shouldBe` Just res4set
    it "#6" $ e (mkCtx [(nameY, tpX0)]) appLam tpSigma `shouldBe` Just ( Set.fromList([(tpX1, tpX0), (tpX3, tpX2), (TArr tpX1 tpSigma, TArr tpX2 tpX3)]) )

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
    it "#5" $ unpackSubstituion (e emptyCtx lamXX tpSigma) `shouldBe` (TArr tpX0 tpX0)
    it "#6" $ unpackSubstituion (e (Context (Map.fromList [(nameY, tpX0)])) appLam tpSigma) `shouldBe` tpX2