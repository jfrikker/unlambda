module AST (
    AstNode(..)
) where

data AstNode = Apply AstNode AstNode |
    PrintChar Char |
    Constant |
    Continuation |
    Distribute |
    Identity |
    Lazy |
    PrintCC |
    Read |
    Term