module Parser where

import AST
import Parsec (Parsec)

parseUnlambda :: Parsec s AstNode
parseUnlambda = anyChar >>= parseInner
    where parseInner '.' = anyChar <$> PrintChar