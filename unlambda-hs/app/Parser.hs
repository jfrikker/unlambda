module Parser where

import qualified AST
import AST (AstNode)
import Text.Parsec (anyChar, manyTill, newline)
import Text.Parsec.Text (Parser)

parseUnlambda :: Parser AstNode
parseUnlambda = anyChar >>= parseInner
    where parseInner '`' = AST.Apply <$> parseUnlambda <*> parseUnlambda
          parseInner 'c' = return AST.Continuation
          parseInner 'd' = return AST.Lazy
          parseInner 'i' = return AST.Identity
          parseInner 'k' = return AST.Constant
          parseInner 'r' = return $ AST.PrintChar '\n'
          parseInner 's' = return AST.Distribute
          parseInner 'v' = return AST.Term
          parseInner '@' = return AST.Read
          parseInner '|' = return AST.PrintCC
          parseInner '.' = AST.PrintChar <$> anyChar
          parseInner '#' = manyTill anyChar newline >> parseUnlambda
          parseInner _ = parseUnlambda