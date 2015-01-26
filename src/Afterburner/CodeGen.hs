module Afterburner.CodeGen ( compileProgram ) where

import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Text as T
import LLVM.General.AST
import LLVM.General.AST.Instruction
import LLVM.General.AST.Constant
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Linkage
import LLVM.General.AST.Visibility
import LLVM.General.AST.Type
import qualified LLVM.General.Module as LLVM
import qualified LLVM.General.Context as LLVM
import qualified LLVM.General.Target as LLVM

import Afterburner.AST
import Afterburner.Types

compileProgram :: FilePath -> Program -> ExceptT String IO ()
compileProgram outfile prog = do
  defs <- either error return $ runCompileM $ compileProgram' prog
  let llvmModule = defaultModule { moduleName = T.unpack $ programId prog
                                 , moduleDefinitions = defs
                                 }
  either throwError return =<< lift (LLVM.withContext (doIt llvmModule))
  where doIt :: Module -> LLVM.Context -> IO (Either String ())
        doIt llvmModule c =
          mayFail $ LLVM.withModuleFromAST c llvmModule $ \m ->
          mayFail $ LLVM.withDefaultTargetMachine $ \machine ->
          (runExceptT $ LLVM.writeObjectToFile machine (LLVM.File outfile) m)

liftError :: Monad m =>
             ExceptT e m (Either e a)
          -> ExceptT e m a
liftError m = do v <- lift $ runExceptT m
                 case v of Left e           -> throwError e
                           Right (Left e)   -> throwError e
                           Right (Right v') -> return v'

mayFail :: Monad m =>
           ExceptT e m (Either e a) -> m (Either e a)
mayFail = runExceptT . liftError

data CompilerState = CompilerState
                     {
                       stateCounter :: Integer
                     }

data CompilerEnv = CompilerEnv
                   {
                     envVtable :: M.Map Id ()
                   }

type CompileM = ExceptT String (ReaderT CompilerEnv (State CompilerState))

runCompileM :: CompileM a -> Either String a
runCompileM m = evalState (runReaderT (runExceptT m) newEnv) newState
  where newState = CompilerState 0
        newEnv = CompilerEnv mempty

compileProgram' :: Program -> CompileM [Definition]
compileProgram' prog = return [GlobalDefinition mainFunction]
  where mainFunction = Function External Default C [] i32 (Name "main")
                       ([], False) [] Nothing 0 Nothing
                       mainBody
        mainBody =
          [ BasicBlock (Name "body") [] $
            Do $ Ret (Just $ ConstantOperand $ Int 32 42) []
          ]
