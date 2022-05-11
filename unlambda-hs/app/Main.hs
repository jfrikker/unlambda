{-# LANGUAGE FlexibleContexts #-}
module Main where

import AST
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as TLIO
import Parser (parseUnlambda)
import System.Environment (getArgs)
import qualified System.IO as IO
import Text.Parsec.Text (parseFromFile)
import Control.Monad.State (StateT, MonadState (get, put), evalStateT, modify, gets)
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

data InterpreterState = InterpreterState {
    currentChar :: Maybe Char,
    currentContinuation :: Continuation
}

getCh :: MonadState InterpreterState m => m (Maybe Char)
getCh = gets currentChar

setCh :: MonadState InterpreterState m => Maybe Char -> m ()
setCh c = modify $ \s -> s { currentChar = c}

pushCC :: MonadState InterpreterState m => (Continuation -> Continuation) -> m ()
pushCC f = modify $ \s -> s { currentContinuation = f $currentContinuation s }

getCC :: MonadState InterpreterState m => m Continuation
getCC = gets currentContinuation

setCC :: MonadState InterpreterState m => Continuation -> m ()
setCC c = modify $ \s -> s { currentContinuation = c }

run :: AstNode -> IO Value
run prog = evalStateT (eval prog) $ InterpreterState { currentChar = Nothing, currentContinuation = CEnd }

eval :: AstNode -> StateT InterpreterState IO Value
eval (AST.Apply f a) = pushCC (CEvalApply a) >> eval f
eval (AST.PrintChar c) = continu $ VPrintChar c
eval AST.Constant = continu VConstant0
eval AST.Continuation = continu VCreateContinuation
eval AST.Identity = continu VIdentity
eval AST.Distribute = continu VDistribute0
eval AST.Lazy = continu VLazy0
eval AST.Term = continu VTerm
eval AST.Read = continu VRead
eval AST.PrintCC = continu VPrintCC

apply :: Value -> Value -> StateT InterpreterState IO Value
apply (VPrintChar c) arg = liftIO (putStr [c]) >> continu arg
apply VConstant0 arg = continu $ VConstant1 arg
apply (VConstant1 k) arg = continu k
apply (VContinuation c) arg = setCC c >> continu arg
apply VCreateContinuation arg = do
    cc <- getCC
    pushCC $ CApply2 arg
    continu $ VContinuation cc
apply VDistribute0 arg = continu $ VDistribute1 arg
apply (VDistribute1 f1) arg = continu $ VDistribute2 f1 arg
apply (VDistribute2 f1 f2) arg = pushCC (CDistribute f2 arg) >> apply f1 arg
apply VIdentity arg = continu arg
apply VLazy0 arg = continu arg
apply (VLazy1 f) arg = pushCC (CApply1 arg) >> eval f
apply VPrintCC arg = do
    c <- getCh
    res <- liftIO $ maybe (return VTerm) (\c -> putStr [c] >> return arg) c
    continu res
apply VTerm _ = continu VTerm
apply VRead arg = do
    eof <- liftIO isEOF
    if eof
        then setCh Nothing >> apply arg VTerm
        else liftIO getChar >>= setCh . Just >> apply arg VIdentity

continu :: Value -> StateT InterpreterState IO Value
continu v = getCC >>= \cc -> continu' cc v
    where continu' CEnd value = return value
          continu' (CEvalApply arg next) VLazy0 = setCC next >> continu (VLazy1 arg)
          continu' (CEvalApply arg next) value = setCC (CApply2 value next) >> eval arg
          continu' (CApply1 arg next) value = setCC next >> apply value arg
          continu' (CApply2 func next) value = setCC next >> apply func value
          continu' (CDistribute f2 arg next) value = setCC (CApply2 value next) >> apply f2 arg