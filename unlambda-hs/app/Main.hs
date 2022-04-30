module Main where

import AST
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as TLIO
import Parser (parseUnlambda)
import System.Environment (getArgs)
import qualified System.IO as IO
import Text.Parsec.Text (parseFromFile)
import Control.Monad.State.Strict (StateT, MonadState (get, put))
import Control.Monad.Trans (liftIO)
import System.IO (isEOF)

main :: IO ()
main = do
    args <- getArgs
    Right input <- parseFromFile parseUnlambda $ args !! 0
    TLIO.putStrLn $ toLazyText $ showAst input

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
    VLazy1 MaybeEvaluated |
    VRead |
    VPrintCC |
    VTerm deriving (Show)

data Continuation = CApply1 MaybeEvaluated Continuation |
    CApply2 Value Continuation |
    CDistribute Value Value Continuation |
    CLazy0 MaybeEvaluated Continuation |
    CEnd deriving (Show)

data MaybeEvaluated = Evaluated Value |
    Unevaluated AstNode deriving (Show)

step :: MaybeEvaluated -> Continuation -> StateT (Maybe Char) IO (MaybeEvaluated, Continuation)
step (Unevaluated (AST.Apply f a)) continuation = return (Unevaluated f, CApply1 (Unevaluated a) continuation)
step (Unevaluated (AST.PrintChar c)) continuation = return (Evaluated $ VPrintChar c, continuation)
step (Unevaluated AST.Constant) continuation = return (Evaluated VConstant0, continuation)
step (Unevaluated AST.Continuation) continuation = return (Evaluated VCreateContinuation, continuation)
step (Unevaluated AST.Identity) continuation = return (Evaluated VIdentity, continuation)
step (Unevaluated AST.Distribute) continuation = return (Evaluated VDistribute0, continuation)
step (Unevaluated AST.Lazy) continuation = return (Evaluated VLazy0, continuation)
step (Unevaluated AST.Term) continuation = return (Evaluated VTerm, continuation)
step (Unevaluated AST.Read) continuation = return (Evaluated VRead, continuation)
step (Unevaluated AST.PrintCC) continuation = return (Evaluated VPrintCC, continuation)
step (Evaluated v) continuation = continu continuation v

maybeApply :: Value -> MaybeEvaluated -> Continuation -> (MaybeEvaluated, Continuation)
maybeApply VLazy0 arg continuation = (Evaluated $ VLazy1 arg, continuation)
maybeApply (VLazy1 f) arg continuation = (arg, CLazy0 f continuation)
maybeApply f arg continuation = (arg, CApply2 f continuation)

applyValue :: Value -> Value -> Continuation -> StateT (Maybe Char) IO (MaybeEvaluated, Continuation)
applyValue (VPrintChar c) arg continuation = liftIO (putStr [c]) >> return (Evaluated arg, continuation)
applyValue VConstant0 arg continuation = return (Evaluated $ VConstant1 arg, continuation)
applyValue (VConstant1 k) arg continuation = return (Evaluated k, continuation)
applyValue (VContinuation c) arg _ = return (Evaluated arg, c)
applyValue VCreateContinuation arg continuation = return (Evaluated $ VContinuation continuation, CApply2 arg continuation)
applyValue VDistribute0 arg continuation = return (Evaluated $ VDistribute1 arg, continuation)
applyValue (VDistribute1 f1) arg continuation = return (Evaluated $ VDistribute2 f1 arg, continuation)
applyValue (VDistribute2 f1 f2) arg continuation = return (Evaluated arg, CApply2 f1 $ CDistribute f2 arg continuation)
applyValue VIdentity arg continuation = return (Evaluated arg, continuation)
applyValue VLazy0 arg continuation = return (Evaluated $ VLazy1 $ Evaluated arg, continuation)
applyValue (VLazy1 f) arg continuation = return (f, CApply1 (Evaluated arg) continuation)
applyValue VPrintCC arg continuation = do
    c <- get
    liftIO $ maybe (return (Evaluated VTerm, continuation)) (\c -> putStr [c] >> return (Evaluated arg, continuation)) c
applyValue VTerm _ continuation = return (Evaluated $ VTerm, continuation)
applyValue VRead arg continuation = do
    eof <- liftIO isEOF
    if eof
        then put Nothing >> applyValue arg VTerm continuation
        else liftIO getChar >>= put . Just >> applyValue arg VIdentity continuation

continu :: Continuation -> Value -> StateT (Maybe Char) IO (MaybeEvaluated, Continuation)
continu CEnd _ = error "Shouldn't have gotten here"
continu (CApply1 arg next) value = return $ maybeApply value arg next
continu (CApply2 func next) value = applyValue func value next
continu (CDistribute f2 arg next) value = return (Evaluated arg, CApply2 f2 $ CApply2 value next)
continu (CLazy0 f next) value = return (f, CApply1 (Evaluated value) next)