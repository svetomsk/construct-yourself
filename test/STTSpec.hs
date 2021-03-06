{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction (Name, Term (..), Type (..), Context (..), Substitution (..)
  , Equation, Substitutable(sub), compose, e, u, termP, typeP)
import           Data.Map as Map (empty, fromList, unionWith, (!))
import           Data.Set as Set (fromList, empty)
import           Test.Hspec
import           Data.Set
import           Data.Maybe as MB (fromJust)
import           Text.Parsec
import           Text.Parsec.Text
import           Data.Text
import           Data.Either (fromRight)
import           Text.Printf (printf)

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

parseTerm :: Text -> Term
parseTerm txt = fromRight (Var "x") (parse termP "parseTerm" txt)

parseType :: Text -> Type
parseType txt = fromRight (TVar "x") (parse typeP "parseType" txt)

pretty :: (Show a, Show b) => Int -> a -> b -> String
pretty number x y = printf "#%i: %s == %s" number (show x) (show y)

itStatement :: (Eq a, Show a) => Int -> a -> a -> SpecWith ()
itStatement num x y = it (pretty num x y) $ x `shouldBe` y

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

tpT = TVar "t"
tpB = TVar "b"
tpC = TVar "c"
tpSigma = TVar nameSigma
tpX = TVar nameX
tpY = TVar nameY
tpX0 = TVar nameX0
tpX1 = TVar nameX1
tpX2 = TVar nameX2
tpX3 = TVar nameX3
tpAlpha = TVar nameAplha
tpBeta = TVar nameBeta
arrType = TArr tpT tpB
tpX2X0 = TArr tpX2 tpX0
tpX020 = TArr tpX0 tpX2X0
tpFail = TVar "fail"
noType = TVar "notype"

emptyCtx = Context Map.empty

contextBT = mkCtx [(nameB, tpT)]
contextBC = mkCtx [(nameB, tpC)]
contextTB = mkCtx [(nameT, tpB)]
contextTC = mkCtx [(nameT, tpC)]
ctxXaYb = mkCtx [(nameX, TArr tpAlpha tpBeta), (nameY, tpAlpha)]
ctxXY = mkCtx [(nameX, tpY)]

emptySubst = Substitution (Map.empty)
substAT = mkSubst [(nameA, tpT)]
substBC = mkSubst [(nameB, tpC)]
substTC = mkSubst [(nameT, tpC)]
substAC = mkSubst [(nameA, tpC)]
substXX0 = mkSubst [(nameX, tpX0)]
substX01 = mkSubst [(nameX0, tpX1)]
substX12 = mkSubst [(nameX1, tpX2)]
substXA01 = mkSubst [(nameX, TArr tpX0 tpX1)]
substX1A23 = mkSubst [(nameX1, TArr tpX2 tpX3)]
substX30 = mkSubst [(nameX3, tpX0)]


varX = parseTerm nameX
varY = parseTerm nameY
lamXY_X = parseTerm "(\\x.(\\y.x))"
appXY = parseTerm "(x y)"
lamXX = parseTerm "(\\x.x)"
appLam = parseTerm "((\\x.x) y)"

res1set = Set.fromList [(tpSigma, tpY)]
res2set = Set.fromList([(tpSigma, (TArr tpX0 tpX1)), (tpX1, (TArr tpX2 tpX3)), (tpX3, tpX0)])
res3set = Set.fromList([(tpX0, tpAlpha), (TArr tpX0 tpSigma, TArr tpAlpha tpBeta)])
res4set = Set.fromList([(tpX1, tpX0), (tpSigma, TArr tpX0 tpX1)])

testContextMonoid :: SpecWith () 
testContextMonoid = do
    itStatement 1 (mappend contextBC mempty) contextBC
    itStatement 2 (mappend contextBC mempty) contextBC
    itStatement 3 (mappend contextBC contextTB) (Context (Map.fromList [(nameB, tpC), (nameT, tpB)]))
    itStatement 4 (mappend mempty mempty) emptyCtx
    itStatement 5 (mappend contextBT contextBC) contextBT

testSubstitutionMonoid :: SpecWith ()
testSubstitutionMonoid = do
    itStatement 1 (mappend mempty substAT) substAT
    itStatement 2 (mappend substAT mempty) substAT
    itStatement 3 (mappend substAT substBC) (mkSubst [(nameA, TVar "t"), (nameB, tpC)])
    itStatement 4 (mappend mempty mempty) (Substitution Map.empty)
    itStatement 5 (mappend substAT substAC) substAT

testSubstInContext :: SpecWith ()
testSubstInContext = do
    itStatement 1 (sub substAT emptyCtx) emptyCtx
    itStatement 2 (sub substAT contextBT) contextBT
    itStatement 3 (sub substBC contextTB) contextTC
    itStatement 4 (sub substBC ( sub substAT contextTB)) contextTC
    itStatement 5 (sub substAT ( sub substBC contextTB)) contextTC

testSubstInType :: SpecWith ()
testSubstInType = do
    itStatement 1 (sub substAT tpT) tpT
    itStatement 2 (sub substBC tpB) tpC
    itStatement 3 (sub substBC arrType) (TArr tpT tpC)
    itStatement 4 (sub substBC (TArr arrType tpB)) (TArr (TArr tpT tpC) tpC)
    itStatement 5 (sub substBC (TArr arrType (TArr tpC tpB))) (TArr (TArr tpT tpC) (TArr tpC tpC))

testCompose :: SpecWith ()
testCompose = do
    itStatement 1 (compose emptySubst substAT) substAT
    itStatement 2 (compose substAT substBC) (Substitution (Map.fromList[(nameB, tpC), (nameA, tpT)]))
    itStatement 3 (compose substTC substAT) (Substitution (Map.fromList[(nameA, tpC), (nameT, tpC)]))
    itStatement 4 (compose substX12 (compose substX01 substXX0)) (Substitution (Map.fromList [(nameX, tpX2), (nameX0, tpX2), (nameX1, tpX2)]))
    itStatement 5 (compose substX30 (compose substX1A23 substXA01)) (Substitution (Map.fromList [(nameX3, tpX0), (nameX1, TArr tpX2 tpX0), (nameX, TArr tpX0 (TArr tpX2 tpX0))]))

testE :: SpecWith ()
testE = do
    itStatement 1 (e ctxXY varX tpSigma) (Just res1set)
    itStatement 2 (e emptyCtx lamXY_X tpSigma) (Just res2set)
    itStatement 3 (e ctxXaYb appXY tpSigma) (Just res3set)
    itStatement 4 (e emptyCtx appXY tpSigma) Nothing
    itStatement 5 (e emptyCtx lamXX tpSigma) (Just res4set)
    itStatement 6 (e (mkCtx [(nameY, tpX0)]) appLam tpSigma) (Just ( Set.fromList([(tpX1, tpX0), (tpX3, tpX2), (TArr tpX1 tpSigma, TArr tpX2 tpX3)]) ))

unpackSubstituion :: Maybe (Set Equation) -> Type
unpackSubstituion set = case set of
                            Nothing -> noType
                            Just a -> let solution = u $ MB.fromJust set
                                        in case solution of
                                            Just s -> (getSubs s) Map.! nameSigma
                                            Nothing -> tpFail

testU :: SpecWith ()
testU = do
    itStatement 1 (unpackSubstituion (e ctxXY varX tpSigma)) tpY
    itStatement 2 (unpackSubstituion (e emptyCtx lamXY_X tpSigma)) tpX020
    itStatement 3 (unpackSubstituion (e ctxXaYb appXY tpSigma)) tpBeta
    itStatement 4 (unpackSubstituion (e emptyCtx appXY tpSigma)) noType
    itStatement 5 (unpackSubstituion (e emptyCtx lamXX tpSigma)) (TArr tpX0 tpX0)
    itStatement 6 (unpackSubstituion (e (Context (Map.fromList [(nameY, tpX0)])) appLam tpSigma)) tpX2