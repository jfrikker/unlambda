module AST (
    AstNode(..),
    showAst
) where
import Data.Text.Lazy.Builder (Builder, singleton)

data AstNode = Apply AstNode AstNode |
    PrintChar Char |
    Constant |
    Continuation |
    Distribute |
    Identity |
    Lazy |
    PrintCC |
    Read |
    Term deriving (Show)

showAst :: AstNode -> Builder 
showAst (Apply arg1 arg2) = singleton '`' <> showAst arg1 <> showAst arg2
showAst (PrintChar '\n') = singleton 'r'
showAst (PrintChar c) = singleton '.' <> singleton c
showAst Constant = singleton 'k'
showAst Continuation = singleton 'c'
showAst Distribute = singleton 's'
showAst Identity = singleton 'i'
showAst Lazy = singleton 'd'
showAst PrintCC = singleton '|'
showAst Read = singleton '@'
showAst Term = singleton 'v'