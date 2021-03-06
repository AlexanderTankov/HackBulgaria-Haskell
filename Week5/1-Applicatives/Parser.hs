module Parser where


import Control.Applicative
import qualified Control.Arrow as A (first)
-- first (+7) (35, 0) -> (42, 0)
import qualified Control.Monad as M ((<=<))


newtype Parser a =
  Parser { parse :: String -> Maybe (a, String) }


instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (A.first f) . p

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  Parser fs <*> p = Parser $ \s ->
    case fs s of
    Just (f, s') -> parse (fmap f p) s'
    Nothing      -> Nothing
-- The same thing, but using `Monad Maybe`
--Parser fs <*> p = Parser $ uncurry (parse . flip fmap p) M.<=< fs


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s ->
  case s of 
  (c:cs) -> if p c then Just (c, cs) else Nothing
  _      -> Nothing

char :: Char -> Parser Char
char = satisfy . (==)

var :: Parser String
var = str "var"

openingBrace :: Parser Char
openingBrace = char '('

closingBrace :: Parser Char
closingBrace = char ')'

inBraces :: Parser a -> Parser a
inBraces p = (openingBrace *> spaces) *> p <* (spaces <* closingBrace)

openingBracket :: Parser Char
openingBracket = char '{'

closingBracket :: Parser Char
closingBracket = char '}'

endingLine :: Parser Char
endingLine = char ';'

inBrackets :: Parser a -> Parser a
inBrackets p = (openingBracket *> spaces) *> p <* (spaces <* closingBracket)

str :: String -> Parser String
str ""     = pure ""
str (x:xs) = liftA2 (:) (char x) $ str xs

space :: Parser Char
space = char ' ' <|> char '\t' <|> char '\n'

spaces :: Parser String
spaces  = zeroOrMore space

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser l <|> Parser r = Parser $ \s -> l s <|> r s

oneOrMore, zeroOrMore  :: Parser a -> Parser [a]
oneOrMore p  = liftA2 (:) p $ zeroOrMore p

zeroOrMore p = oneOrMore p <|> pure []