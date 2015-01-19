type Name = String
type TypeVar = String
type TypeCon = String

data Expr
  = Lam Type Name Expr    -- \x -> a
  | Var Name              -- x
  | App Expr Expr         -- a b
  | TLam Name Expr        -- /\ a . b
  | TApp Expr Type        -- a [ b ]
  | Lit Literal           -- 1
  | Let Name Expr Expr    -- let x = v in a

data Type
  = TForall [Name] Type
  | TArr Type Type
  | TCon TypeCon
  | TVar TypeVar
  deriving (Show)

data Literal
  = LitInt Integer
  | LitChar Char
  deriving (Eq, Ord, Show)
