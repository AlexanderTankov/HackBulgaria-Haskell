module Main where

import System.Environment (getArgs)
import StatementParser (interpret)
import Interpreter (runProgram)
import Parser (parse)
import Control.Arrow (first)
import Control.Applicative


main :: IO ()
