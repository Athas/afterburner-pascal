module Main (main) where

import Control.Monad.Except
import System.Environment
import qualified Data.Text.IO as T
import System.FilePath
import System.Process
import System.Exit

import Afterburner.Parser
import Afterburner.CodeGen

main :: IO ()
main = either error return <=< runExceptT $ do
  args <- lift getArgs
  case args of
    [progfile] -> do
      outfile <- getOutFile progfile
      s <- lift $ T.readFile progfile
      case parseProgram progfile s of
        Left e -> throwError $ show e
        Right prog -> do
          compileProgram outfile prog
          ret <- lift $ rawSystem "clang"
                 [outfile, "-o", outfile `replaceExtension` ""]
          case ret of ExitSuccess -> return ()
                      ExitFailure code ->
                        throwError $ "clang failed with error code " ++ show code
    _ -> throwError "Give me a single argument"

getOutFile :: Monad m =>
              FilePath -> ExceptT String m FilePath
getOutFile path
  | takeExtension path == ".pas" =
    return $ path `replaceExtension` ".o"
  | otherwise =
    throwError $ path ++ " has no .pas extension, and thus cannot possibly be a Pascal program"
