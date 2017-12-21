{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction (Name, Term (..), appP, varP, lamP, termP, Type (..), tVarP, tArrP, typeP)
import           Test.Hspec
import Text.Parsec
import Text.Parsec.Text
import Data.Text 


main :: IO ()
main = hspec $ do
    describe "Parser test" parserTest
    describe "Type parser test" typeParserTest
    
parserTest :: SpecWith ()
parserTest = do
  let varX  = Var "x"
      varX1 = Var "x1"
      varX2 = Var "x2"

      combined1 = App (Lam "x" varX) varX1
      combined2 = Lam "x2" (App (Lam "x" varX) varX1)

  it "should test var parser" $ do
    check varP "x" varX
    check varP "x2" varX2
    
  it "should test app parser" $ do
    let app1  = App varX varX1
        app2  = App varX1 varX2
        app3  = App varX (App varX1 varX2)
        app4  = App (App varX varX1) varX2
    check termP "(x x1)" app1
    check termP "(x     x1)" app1
    check termP "(x1 x2)" app2
    check termP "(x (x1 x2))" app3
    check termP "((x x1) x2)" app4
    
  it "should test lam parser" $ do
    let lam1 = Lam "x" varX
        lam2 = Lam "x" varX1
        lam3 = Lam "x" (App varX varX1)  
    check termP "(\\x.x)" lam1
    check termP "(\\x.x1)" lam2
    check termP "(\\x.(x x1))" lam3
    check termP "((\\x.x) x1)" combined1

  it "should test bracket parser" $ do
    check termP "((\\x.x)  (x1))" combined1 
    check termP "((((\\x2.((\\x.x) x1)))))" combined2


tpX = TVar "x"
tpY = TVar "y"
tpXY = TArr tpX tpY

typeParserTest :: SpecWith ()
typeParserTest = do
  it "#1" $ check typeP "x" tpX
  it "#2" $ check typeP "x ->  y" tpXY
  it "#3" $ check typeP "x -> x -> y" (TArr tpX (TArr tpX tpY))


check :: (Eq a, Show a) => Parser a -> Text -> a -> Expectation
check parser inputStr result =
  parse parser "term parser" inputStr `shouldBe` Right result
