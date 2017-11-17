{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Tasks.GADT_1.GADTParser where

import           Data.Text              (pack)
import           Tasks.GADT_1.GADTExpr
import           Text.Parsec.Char       (char, digit, space, string)
import           Text.Parsec.Combinator (between, many1)
import           Text.Parsec.Language   (haskellDef)
import           Text.Parsec.Prim       (many, parseTest, try, (<|>))
import           Text.Parsec.Text       (Parser)
import           Text.Parsec.Token

iLitP :: Parser (Lit Int)
iLitP = undefined

bLitP :: Parser (Lit Bool)
bLitP = undefined

iiLitP :: Parser (Expr Int)
iiLitP = Lit <$> iLitP

bbLitP :: Parser (Expr Bool)
bbLitP = Lit <$> bLitP

addP :: Parser (Expr Int)
addP = undefined

leqP :: Parser (Expr Bool)
leqP = Leq <$> (parse <* char '<') <*> parse

andP :: Parser (Expr Bool)
andP = undefined

spacedP :: Parser a -> Parser a
spacedP p = (many space *> p) <* many space

bracketP :: Parser a -> Parser a
bracketP = undefined

class MyParse a where
  parse :: Parser (Expr a)

instance MyParse Int where
  parse = undefined
instance MyParse Bool where
  parse = undefined
