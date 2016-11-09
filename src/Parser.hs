-- | Parser for glob expressions
module Parser where

import Data.Char
import Data.List (foldr)
import qualified Data.Set as S
import Types

-- |
-- Determine whether a parse result is an error
isErrorResult :: ParseResult a -> Bool
isErrorResult (ErrorResult _) = True
isErrorResult _ = False

-- |
-- Parser that always fails with @UnexpectedChar@ using the given character
unexpectedChar :: Char -> Parser a
unexpectedChar c = P (\_ -> ErrorResult (UnexpectedChar c))

-- |
-- Parser that always succeeds with the given value an consumes no input
constParser :: a -> Parser a
constParser = pure

-- |
-- Parser that always fails and consumes no input
failedParser :: Parser a
failedParser = P (\_ -> ErrorResult Failed)

-- |
-- Parser that continues producing a list of values from the given parser
many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

-- |
-- Parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser
-- (to ultimately produce a non-empty list)
many1 :: Parser a -> Parser [a]
many1 p = p >>= \q -> (q :) <$> many p

-- |
-- Parser that produces a character but fails if
--   * The input is empty.
--   * The character does not satisfy the given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
  character >>=
  (\c ->
     if f c
       then constParser c
       else unexpectedChar c)

-- |
-- Parser that succeeds with a character off the input or fails with
-- an error if the input is empty
character :: Parser Char
character = P f
  where
    f [] = ErrorResult UnexpectedEof
    f (x:xs) = Result xs x

-- |
-- Parser that produces the given character but fails if
--   * The input is empty.
--   * The produced character is not equal to the given character.
oneChar :: Char -> Parser Char
oneChar c = satisfy (c ==)

-- |
-- Parser that produces a character between '0' and '9' but fails if
--   * The input is empty.
--   * The produced character is not a digit.
oneDigit :: Parser Char
oneDigit = satisfy isDigit

-- |
-- Parser that produces a space character but fails if
--   * The input is empty.
--   * The produced character is not a space.
space :: Parser Char
space = satisfy isSpace

-- |
-- Parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--   * The input is empty.
--   * The first produced character is not a space.
spaces1 :: Parser String
spaces1 = many1 space

-- |
-- Parser that consumes zero or more spaces
spaces :: Parser String
spaces = many space

-- |
-- Parser that produces a lower-case character but fails if
--   * The input is empty.
--   * The produced character is not lower-case.
lower :: Parser Char
lower = satisfy isLower

-- |
-- Parser that produces an upper-case character but fails if
--   * The input is empty.
--   * The produced character is not upper-case.
upper :: Parser Char
upper = satisfy isUpper

-- |
-- Parser that produces an alpha character but fails if
--   * The input is empty.
--   * The produced character is not alpha.
alpha :: Parser Char
alpha = satisfy isAlpha

-- |
-- Parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list
sequenceParser :: [Parser a] -> Parser [a]
sequenceParser = foldr (\pa acc -> pa >>= (\a -> (a :) <$> acc)) (pure [])

-- |
-- Parser that produces the given number of values off the given parser.
-- This parser fails if the given parser fails in the attempt to produce
-- the given number of values.
thisMany :: Int -> Parser a -> Parser [a]
thisMany n = sequenceParser . (replicate n)

-- |
-- Parser that parses any character in the given string
oneOf :: String -> Parser Char
oneOf = satisfy . flip elem

-- |
-- Parser that parses any character not in the given string
noneOf :: String -> Parser Char
noneOf = satisfy . flip notElem

-- |
-- Parser that applies the first parser, runs the third parser keeping the result,
-- then runs the second parser and produces the obtained result
between :: Parser l -> Parser r -> Parser a -> Parser a
between pl pr pa = pl *> pa <* pr

-- |
-- Parser that matches a literal character.
-- A literal character is any escaped character '\x' or
-- any non-escaped character except '?', '*', '\' '['.
literal :: Parser Matcher
literal = (unescaped <|> escaped) >>= return . Literal
  where
    special = "?*\\["
    unescaped = noneOf special
    escaped = oneChar '\\' *> character

-- |
-- Parser that applies the first parser as many times as possible
-- until the end parser succeeds
manyTill
  :: Monoid a
  => Parser a -> Parser b -> Parser a
manyTill pa pend = (pa <&> manyTill pa pend) <|> (pend >> constParser mempty)

-- |
-- Parser for a matcher that matches any string
anyString :: Parser Matcher
anyString = oneChar '*' >>= return . (const AnyString)

-- |
-- Parser for a matcher that matches any one character
anyChar :: Parser Matcher
anyChar = oneChar '?' >>= return . (const AnyChar)

-- |
-- Parser for a range of characters
charRange :: Parser String
charRange = do
  l <- character <* oneChar '-'
  r <- character
  return (toEnum <$> [fromEnum l .. fromEnum r])

-- |
-- Parser for a set of characters
charSet :: Parser Matcher
charSet = do
  chars <- oneChar '[' *> manyTill p (oneChar ']')
  return (Set $ S.fromList chars)
  where p = charRange <|> (character >>= return . pure)
