-- Traditional call-by-value interpreter.

data Expr
  = Var Int
  | Lam Expr
  | App Expr Expr
  | Lit Int
  | Prim PrimOp Expr Expr
  deriving Show

data Value
  = VInt Int
  | VClosure Expr Env
  deriving Show

data PrimOp = Add | Mul
  deriving Show

type Env = [Value]

eval :: Env -> Expr -> Value
eval env term = case term of
  Var n -> env !! n
  Lam a -> VClosure a env
  App a b ->
    let VClosure c env' = eval env a in
    let v = eval env b in
    eval (v : env') c

  Lit n -> VInt n
  Prim p a b -> (evalPrim p) (eval env a) (eval env b)

evalPrim :: PrimOp -> Value -> Value -> Value
evalPrim Add (VInt a) (VInt b) = VInt (a + b)
evalPrim Mul (VInt a) (VInt b) = VInt (a + b)

emptyEnv :: Env
emptyEnv = []

-- (\x y -> x + y) 10 20
test :: Value
test = eval emptyEnv $ App (App (Lam (Lam (Prim Add (Var 0) (Var 1)))) (Lit 10)) (Lit 20)
