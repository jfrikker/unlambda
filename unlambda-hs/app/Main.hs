{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import AST
import Prelude hiding (readFile)
import System.Environment (getArgs)
import Data.Text.IO (readFile)
import qualified System.IO as IO

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ args !! 0
    return ()
