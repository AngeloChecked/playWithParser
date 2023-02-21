{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module UriParser where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Identity
import Text.Megaparsec.Debug

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
  parseTest pUri "irc"
  parseTest pUri "http:"
  parseTest pUri "https:"
  parseTest pUri "mailto:"
  parseTest pUri "foo:"
  parseTest alternatives "ab"
  parseTest alternatives "ac"
  parseTest tryAlternatives "ab"
  parseTest tryAlternatives "ac"
  parseTest (pUri <* eof) "https://mark:secret@example.com"
  parseTest (pUri <* eof) "https://mark:secret@example.com:123"
  parseTest (pUri <* eof) "https://example.com:123"
  parseTest (pUri <* eof) "https://mark@example.com:123"
  parseTest (pUri <* eof) "https://mark:@example.com"
  parseTest (pUri' <* eof) "https://mark:@example.com"
  parseTest (pUri'' <* eof) "https://mark:@example.com"
  parseTest (many (char 'a') >> many (char 'b') >> eof :: Parser ()) "d"
  parseTest (many (char 'a') >> hidden (many (char 'b')) >> eof :: Parser ()) "d"

alternatives :: Parser (Char, Char)
alternatives = foo <|> bar
  where
    foo :: ParsecT Void Text Identity (Char, Char) 
    foo = (,) <$> char 'a' <*> char 'b'
    bar = (,) <$> char 'a' <*> char 'c'

tryAlternatives :: Parser (Char, Char)
tryAlternatives = try foo <|> bar
  where
    foo :: ParsecT Void Text Identity (Char, Char) 
    foo = (,) <$> char 'a' <*> char 'b'
    bar = (,) <$> char 'a' <*> char 'c'

newtype Uri' = Uri'
  { uriScheme' :: Text
  } deriving (Eq, Show)

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

pScheme'' :: Parser Scheme
pScheme'' = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttps  <$ string "https"
  , SchemeHttp   <$ string "http"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]

data Uri = Uri
  { uriScheme :: Scheme
  , uriAuthority :: Maybe Authority
  } deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (Text, Text) -- (user, password)
  , authHost :: Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)
  
pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme''
  void (char ':')
  uriAuthority <- optional . try $ do            -- (1)
    void (string "//")
    authUser <- optional . try $ do              -- (2)
      user <- T.pack <$> some alphaNumChar       -- (3)
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.')
    authPort <- optional (char ':' *> L.decimal) -- (4)
    return Authority {..}                        -- (5)
  return Uri {..}         


pUri'' :: Parser Uri
pUri'' = do
  uriScheme <- pScheme''
  void (char ':')
  uriAuthority <- optional $ do -- removed 'try' on this line
    void (string "//")
    authUser <- optional . try $ do
      user <- T.pack <$> some alphaNumChar
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.')
    authPort <- optional (char ':' *> L.decimal)
    return Authority {..}
  return Uri {..}

pUri' :: Parser Uri
pUri' = do
  uriScheme <- dbg "scheme" pScheme''
  void (char ':')
  uriAuthority <- dbg "auth" . optional . try $ do
    void (string "//")
    authUser <- dbg "user" . optional . try $ do
      user <- T.pack <$> some alphaNumChar
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> dbg "host" (some (alphaNumChar <|> char '.'))
    authPort <- dbg "port" $ optional (char ':' *> L.decimal)
    return Authority {..}
  return Uri {..}

