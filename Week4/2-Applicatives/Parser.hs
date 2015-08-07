-- Task 3
module Parser where


import Control.Applicative

newtype Parser a =
  Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
--fmap :: (a -> b) -> fa -> fb
--  fmap f (Parser p) = Parser (f b)
--    where b (x:xs) = Just (x, xs)

instance Applicative Parser where
  pure  = undefined
  (<*>) = undefined

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser b
    where b (x:xs) = if f x then Just (x, xs) else Nothing
          b _ = Nothing

char :: Char -> Parser Char
char c = satisfy (c ==)

openingBrace :: Parser Char
openingBrace = char '('

closingBrace :: Parser Char
closingBrace = char ')'

inBraces :: Parser a -> Parser a
inBraces p = openingBrace *> p <* closingBrace

openingBracket :: Parser Char
openingBracket = char '{'

closingBracket :: Parser Char
closingBracket = char '}'

inBrackets :: Parser a -> Parser a
inBrackets p = openingBracket *> p <* closingBracket

oneOrMore, zeroOrMore  :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p)

zeroOrMore = oneOrMore <|> pure []