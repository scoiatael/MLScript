module Lib
    ( module Parser,
      module Interpreter
    ) where

import Parser (parseToplevel, parseExpr)

import Interpreter (eval)
