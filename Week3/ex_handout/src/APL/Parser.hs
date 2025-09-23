module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

-- Do not change this definition.
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

-- use some to run (satisfy isDigit) parser, which parses one digit.
-- some will keep parsing until it fails, and collect the numbers in a list
-- Then we check to make sure the next thing is not a character, then remove white space
lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)

keywords :: [String]
keywords = ["true", "false"]

lVName :: Parser VName
lVName = lexeme $ try $ do
  a <- satisfy isAlpha
  as <- many $ satisfy isAlphaNum
  let vn = a : as in
    if elem vn keywords
    then fail "Unexpected keyword"
    else pure $ a : as

pBool :: Parser Bool
pBool = choice
          [ lKeyword "true" >> pure True,
            lKeyword "false" >> pure False
          ]

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pExp0 :: Parser Exp
pExp0 = do
  x <- pAtom
  chain x
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pAtom
            chain $ Add x y,
          do
            lString "-"
            y <- pAtom
            chain $ Sub x y,
          do
            lString "*"
            y <- pAtom
            chain $ Mul x y,
          do
            lString "/"
            y <- pAtom
            chain $ Div x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0

-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
