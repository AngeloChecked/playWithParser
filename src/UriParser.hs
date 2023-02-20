{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module UriParser where

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

-- scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]

type Parser = Parsec Void Text

pScheme :: Parser Text
pScheme = string "data"
  <|> string "file"
  <|> string "ftp"
  <|> string "http"
  <|> string "https"
  <|> string "irc"
  <|> string "mailto"

pScheme' :: Parser Text
pScheme' = choice
  [ string "data"
  , string "file"
  , string "ftp"
  , string "http"
  , string "https"
  , string "irc"
  , string "mailto" ]

uriParserMain :: IO ()
uriParserMain = do
  parseTest pScheme ""
  parseTest pScheme "dat"
  parseTest pScheme "file"
  parseTest pScheme "irc"
  parseTest pScheme' "irc"
