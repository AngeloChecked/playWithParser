{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ShuntingYardMega where

import Control.Applicative hiding (many, some)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec (Parsec, try)
import Text.Megaparsec.Char (char, numberChar)
import Control.Monad.Combinators (some, optional)
import Data.Functor (void)
import Expr

type Parser = Parsec Void Text

parseShutingYard :: Parser (Expr Integer)
parseShutingYard = do
  x <- try pAdd <|> pLit  
  return x

pAdd :: Parser (Expr Integer)
pAdd = do
  lit1 <- pLit
  void $ optional (char ' ')
  void (char '+')
  void $ optional (char ' ')
  Add lit1 <$> pLit
  
pMult :: Parser (Expr Integer)
pMult = do
  lit1 <- pLit
  void $ optional (char ' ')
  void (char '*')
  void $ optional (char ' ')
  Mult lit1 <$> pLit

pLit :: Parser (Expr Integer)
pLit = do 
  lit <- some numberChar
  return (Lit $ read lit)

-- op :: parser (expr integer)
-- op = choice
--   [ add  <$ char '+'
--   , mult <$ char '*'
--   ]

