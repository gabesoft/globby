-- | Data types
module Types where

import Data.Monoid
import qualified Data.Set as S

data MatcherType
  = Literal Char
  | AnyString
  | AnyChar
  | Set (S.Set Char)
  deriving (Eq)

instance Show MatcherType where
  show (Literal c) = escapeSpecial c
  show AnyString = "*"
  show AnyChar = "?"
  show (Set s) = "[" ++ (concat $ escapeSetSpecial <$> S.toList s) ++ "]"

escapeSpecial :: Char -> String
escapeSpecial '*' = "\\*"
escapeSpecial '?' = "\\?"
escapeSpecial '[' = "\\["
escapeSpecial c = [c]

escapeSetSpecial :: Char -> String
escapeSetSpecial ']' = "\\]"
escapeSetSpecial c = [c]

type Input = String

data ParseError
  = UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | Failed
  deriving (Eq)

instance Show ParseError where
  show UnexpectedEof = "Unexpected end of stream"
  show (UnexpectedChar c) = "Unexpected character >" ++ show c ++ "<"
  show Failed = "Parse failed"
  show (ExpectedEof input) =
    "Expected end of stream, but got >" ++ show input ++ "<"

data ParseResult a
  = ErrorResult ParseError
  | Result Input
           a
  deriving (Eq)

data MatcherResult =
  MResult [Input]
          Bool
  deriving (Eq, Show)

instance Show a =>
         Show (ParseResult a) where
  show (ErrorResult e) = show e
  show (Result input a) = concat ["Result >", input, "< ", show a]

data Parser a = P
  { parse :: Input -> ParseResult a
  }

data Matcher = M
  { match :: Input -> MatcherResult
  }

instance Monoid MatcherResult where
  mempty = MResult [] False
  mappend (MResult xs b1) (MResult ys b2) = MResult (xs <> ys) (b1 || b2)

instance Monoid Matcher where
  mempty = M (\xs -> MResult [xs] True)
  mappend (M f) (M g) =
    M
      (\xs ->
         let (MResult ys m) = f xs
         in if m
              then foldr mappend mempty (g <$> ys)
              else MResult [xs] False)

-- |
-- Functor instance for a @Parser@.
instance Functor Parser where
  fmap f (P fa) =
    P
      (\x ->
         case fa x of
           ErrorResult e -> ErrorResult e
           Result input a -> Result input (f a))

-- |
-- Applicative instance for a @Parser@.
instance Applicative Parser where
  pure a = P (\x -> Result x a)
  (<*>) pf pa = flip bindParser pf (\f -> f <$> pa)

-- |
-- Monad instance for a @Parser@.
instance Monad Parser where
  (>>=) = flip bindParser

bindParser :: (a -> Parser b) -> Parser a -> Parser b
bindParser f (P pa) = P g
  where
    g input =
      case pa input of
        ErrorResult e -> ErrorResult e
        Result i a -> parse (f a) i

-- |
-- Parser that applies the first parser and then the second and
-- combines the results. An error is returned if any parser fails.
(<&>)
  :: Monoid a
  => Parser a -> Parser a -> Parser a
(<&>) pa1 pa2 = do
  a1 <- pa1
  a2 <- pa2
  return (mappend a1 a2)

-- |
-- Parser that tries the first parser for a successful value.
--   * If the first parser succeeds then use this parser.
--   * If the first parser fails, try the second parser.
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) (P pa1) (P pa2) = P f
  where
    f input =
      case pa1 input of
        ErrorResult _ -> pa2 input
        r -> r

infixl 3 <|>

infixl 4 <&>
