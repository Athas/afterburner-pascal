module Main (main) where

import System.Environment
import qualified Data.Text.IO as T

import Afterburner.Parser

main :: IO ()
main = do args <- getArgs
          case args of
            [progfile] -> do s <- T.readFile progfile
                             case parseProgram progfile s of
                               Left e -> error $ show e
                               Right prog -> print prog
            _ -> error "Give me a single argument"
