module StatementParser where

import Parser
import Statement
import Control.Applicative
import qualified Data.Char as C
  ( isAlpha
  , isDigit
  )

str :: String -> Parser String
str ""     = pure ""
str (x:xs) = liftA2 (:) (char x) $ str xs

plusP, minusP, multP, divP, modP, gtP, geP, ltP, leP, eqlP :: Parser Oper

plusP = const Plus <$> str "+"

minusP = const Minus <$> str "-"

multP = const Mult <$> str "*"

divP = const Div <$> str "div"

modP = const Mod <$> str "mod"

gtP = const Gt <$> str ">"

geP = const Ge <$> str ">="

ltP = const Lt <$> str "<"

leP = const Le <$> str "<="

eqlP  = const Eql <$> str "=="

oper :: Parser Oper
oper = plusP <|> minusP <|> multP <|> divP <|> modP <|> gtP <|> geP <|> ltP <|> leP <|> eqlP

variable :: Parser Variable
variable = oneOrMore $ satisfy C.isAlpha

value :: Parser Value
value = read <$> (oneOrMore $ satisfy C.isDigit)

expr :: Parser Expr
expr = Var <$> variable
       <|> Val <$> value
       <|> liftA3 Op expr oper expr

assignP :: Parser Statement
assignP = liftA2 Assign (variable <* char '=') expr

incrP :: Parser Statement
incrP = Incr <$> (str "++" *> variable)

decrP :: Parser Statement
decrP = Decr <$> (str "--" *> variable)

ifP :: Parser Statement
ifP = str "if" *> liftA2 If (inBraces expr) (inBrackets statement)

forP :: Parser Statement
forP = str "for" *> (For <$> (openingBrace *> statement) <*> (str ";" *> expr) <*> (str ";" *> statement <* closingBrace) <*> (inBrackets statement))

interpret :: Parser [Statement]
interpret = oneOrMore statement

statement :: Parser Statement
statement = assignP <|> incrP <|> decrP <|> ifP <|> forP