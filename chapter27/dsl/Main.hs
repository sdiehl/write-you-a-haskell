import JIT
import Codegen
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.Constant as C

{-

; ModuleID = 'my cool jit'

define double @main() {
entry:
  %1 = fadd double 1.000000e+01, 2.000000e+01
  ret double %1
}

-}

initModule :: AST.Module
initModule = emptyModule "my cool jit"

logic = do
  define double "main" [] $ do
    let a = cons $ C.Float (F.Double 10)
    let b = cons $ C.Float (F.Double 20)
    res <- fadd a b
    ret res

main = do
  let ast = runLLVM initModule logic
  runJIT ast
  return ast
