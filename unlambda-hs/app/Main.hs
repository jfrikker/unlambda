module Main where

import AST
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as TLIO
import Parser (parseUnlambda)
import System.Environment (getArgs)
import qualified System.IO as IO
import Text.Parsec.Text (parseFromFile)
import Control.Monad.State.Strict (StateT, MonadState (get, put), evalStateT)
import Control.Monad.Trans (liftIO)
import System.IO (isEOF)

main :: IO ()
main = do
    args <- getArgs
    Right input <- parseFromFile parseUnlambda $ args !! 0
    TLIO.putStrLn $ toLazyText $ showAst input
    out <- run input
    print out

data Value = VPrintChar Char |
    VConstant0 |
    VConstant1 Value |
    VContinuation Continuation |
    VCreateContinuation |
    VDistribute0 |
    VDistribute1 Value |
    VDistribute2 Value Value |
    VIdentity |
    VLazy0 |
    VLazy1 AstNode |
    VRead |
    VPrintCC |
    VTerm deriving (Show)

data Continuation = CEvalApply AstNode Continuation |
    CApply1 Value Continuation |
    CApply2 Value Continuation |
    CDistribute Value Value Continuation |
    CEnd deriving (Show)

run :: AstNode -> IO Value
run prog = evalStateT (eval prog CEnd) Nothing

eval :: AstNode -> Continuation -> StateT (Maybe Char) IO Value
eval (AST.Apply f a) continuation = eval f $ CEvalApply a continuation
eval (AST.PrintChar c) continuation = continu continuation $ VPrintChar c
eval AST.Constant continuation = continu continuation VConstant0
eval AST.Continuation continuation = continu continuation VCreateContinuation
eval AST.Identity continuation = continu continuation VIdentity
eval AST.Distribute continuation = continu continuation VDistribute0
eval AST.Lazy continuation = continu continuation VLazy0
eval AST.Term continuation = continu continuation VTerm
eval AST.Read continuation = continu continuation VRead
eval AST.PrintCC continuation = continu continuation VPrintCC

apply :: Value -> Value -> Continuation -> StateT (Maybe Char) IO Value
apply (VPrintChar c) arg continuation = liftIO (putStr [c]) >> continu continuation arg
apply VConstant0 arg continuation = continu continuation $ VConstant1 arg
apply (VConstant1 k) arg continuation = continu continuation k
apply (VContinuation c) arg _ = continu c arg
apply VCreateContinuation arg continuation = continu (CApply2 arg continuation) $ VContinuation continuation
apply VDistribute0 arg continuation = continu continuation $ VDistribute1 arg
apply (VDistribute1 f1) arg continuation = continu continuation $ VDistribute2 f1 arg
apply (VDistribute2 f1 f2) arg continuation = continu (CDistribute f2 arg continuation) arg
apply VIdentity arg continuation = continu continuation arg
apply VLazy0 arg continuation = continu continuation arg
apply (VLazy1 f) arg continuation = eval f $ CApply1 arg continuation
apply VPrintCC arg continuation = do
    c <- get
    res <- liftIO $ maybe (return VTerm) (\c -> putStr [c] >> return arg) c
    continu continuation res
apply VTerm _ continuation = continu continuation VTerm
apply VRead arg continuation = do
    eof <- liftIO isEOF
    if eof
        then put Nothing >> apply arg VTerm continuation
        else liftIO getChar >>= put . Just >> apply arg VIdentity continuation

continu :: Continuation -> Value -> StateT (Maybe Char) IO Value
continu CEnd value = return value
continu (CEvalApply arg next) VLazy0 = continu next $ VLazy1 arg
continu (CEvalApply arg next) value = eval arg $ CApply2 value next
continu (CApply1 arg next) value = apply value arg next
continu (CApply2 func next) value = apply func value next
continu (CDistribute f2 arg next) value = apply f2 arg $ CApply2 value next