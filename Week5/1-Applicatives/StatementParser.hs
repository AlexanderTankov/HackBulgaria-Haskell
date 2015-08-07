module StatementParser where

import Parser
import Statement
import Control.Applicative
import qualified Data.Char as C
  ( isAlpha
  , isDigit
  )

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
variable = ((var <* spaces) <|> spaces) *> (oneOrMore $ satisfy C.isAlpha)

value :: Parser Value
value = read <$> (oneOrMore $ satisfy C.isDigit)

opP :: Parser Expr
opP = liftA3 Op (varP <|> valP <|> inBraces opP) (spaces *> oper <* spaces) (varP <|> valP <|> inBraces opP)

valP :: Parser Expr
valP = Val <$> value

varP :: Parser Expr
varP = Var <$> variable

expr :: Parser Expr
expr = opP <|> valP <|> varP

assignP :: Parser Statement
assignP = liftA2 Assign (variable <* (spaces *> char '=' <* spaces)) expr

incrP :: Parser Statement
incrP = Incr <$> (str "++" *> variable)

decrP :: Parser Statement
decrP = Decr <$> (str "--" *> variable)

ifP :: Parser Statement
ifP = str "if" *> liftA2 If (spaces *> (inBraces expr) <* spaces) ((inBrackets statement) <* spaces)

forP :: Parser Statement
forP = str "for" *> (inBraces(liftA3 For statement (spaces *> (expr <* endingLine) <* spaces) (assignP <|> incrP <|> decrP)) <* spaces) <*> ((inBrackets statement) <* spaces)

interpret :: Parser [Statement]
interpret = oneOrMore statement

statement :: Parser Statement
statement = assignP <* (endingLine <* spaces) <|> incrP <* (endingLine <* spaces) <|> decrP <* (endingLine <* spaces) <|> ifP <|> forP

main = do
    content <- readFile "test"
    print $ parse interpret content
