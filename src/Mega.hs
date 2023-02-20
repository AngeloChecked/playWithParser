{-# LANGUAGE OverloadedStrings #-}
module Mega where 

import Text.Megaparsec
import Data.Void
import Data.Text
import Text.Megaparsec.Char (char, string, string')

type Parser = Parsec Void Text

mySequence :: Parser (Char, Char, Char)
mySequence = do
  a <- char 'a'
  b <- char 'b'
  c <- char 'c'
  return (a, b, c)

mySequence' :: Parser (Char, Char, Char)
mySequence' =
  (,,) <$> char 'a'
       <*> char 'b'
       <*> char 'c'

megaMain :: IO ()
megaMain = do
  parseTest (satisfy (== 'a') :: Parser Char) ""
  parseTest (satisfy (== 'a') :: Parser Char) "a"
  parseTest (satisfy (== 'a') :: Parser Char) "b"
  parseTest (satisfy (> 'c') :: Parser Char) "a"
  parseTest (satisfy (> 'c') :: Parser Char) "d"
  parseTest (char 'a' :: Parser Char) "b"
  parseTest (char 'a' :: Parser Char) "a"
  parseTest (string "foo" :: Parser Text) "foo"
  parseTest (string "foo" :: Parser Text) "bar"
  parseTest (string' "foo" :: Parser Text) "FOO"
  parseTest (string' "foo" :: Parser Text) "FoO"
  parseTest (string' "foo" :: Parser Text) "FoZ"
  parseTest mySequence "abc"
  parseTest mySequence "bcd"
  parseTest mySequence "bcd"
  parseTest mySequence' "abc"
  parseTest (many (char 'a') :: Parser [Char]) "aaa"
  parseTest (many (char 'a') :: Parser [Char]) "aabbb"
  parseTest (many (char 'a') <* eof :: Parser [Char]) "aabbb"
	
	