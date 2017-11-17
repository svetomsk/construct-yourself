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
iLitP = ILit <$> (read::String->Int) <$> (many digit)

bLitP :: Parser (Lit Bool)
bLitP = BLit <$> (\x -> if x == 'F' then False else True) <$> ((char 'F') <|> (char 'T')) 

iiLitP :: Parser (Expr Int)
iiLitP = spacedP $ try $ Lit <$> iLitP

bbLitP :: Parser (Expr Bool)
bbLitP = spacedP $ try $ Lit <$> bLitP

addP :: Parser (Expr Int)
addP = spacedP $ try $ Add <$> (bracketP addP <|> bracketP iiLitP <|> iiLitP) <*> (((many space *> char '+') <* many space) *> parse)

leqP :: Parser (Expr Bool)
leqP = spacedP $ try $ Leq <$> (bracketP addP <|> addP <|> bracketP iiLitP <|> iiLitP) <*> (((many space *> (char '<')) *> many space) *> parse)

andP :: Parser (Expr Bool)
andP = spacedP $ try $ And <$> (bracketP leqP <|> bracketP bbLitP <|> bbLitP) <*> ((many space *> (((char '&') *> (char '&')) *> many space)) *> parse)

spacedP :: Parser a -> Parser a
spacedP p = (many space *> p) <* many space

bracketP :: Parser a -> Parser a
bracketP p = spacedP $ try $ between (char '(') (char ')') $ spacedP p

class MyParse a where
  parse :: Parser (Expr a)

instance MyParse Int where
  parse = addP <|> bracketP addP <|> bracketP iiLitP <|> iiLitP

instance MyParse Bool where
  parse = leqP <|> bracketP leqP <|> andP <|> bracketP andP <|> bbLitP <|> bracketP bbLitP
