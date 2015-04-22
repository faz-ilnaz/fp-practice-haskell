import Data.Maybe (fromJust)
 
data Type = Base
		  		| ArrowTy Type Type
					deriving (Eq)
 
instance Show Type where
  show (ArrowTy arg1 arg2) =
      "(" ++ show arg1 ++ " -> " ++ show arg2 ++ ")"
  show (Base) =
      "Base"

data Term = Const Int
				  | Var String
				  | Lam String Type Term
				  | App Term Term
					deriving (Eq)

instance Show Term where
    show (Lam argname ty body) =
        "(\\" ++ argname ++ " : " ++ show ty ++ ". " ++ show body ++ ")"
    show (App function argument) =
        "(" ++ show function ++ " " ++ show argument ++ ")"
    show (Var name) = name
    show (Const val) = show val
 
typeof :: [(String, Type)] -> Term -> Type
typeof _ Const{} = Base
typeof e (Var x) = fromJust (lookup x e)
typeof e (Lam arg ty t) = ArrowTy ty (typeof ((arg, ty):e) t)
typeof e (App t arg) =
	let ArrowTy ty ret = typeof e t in
	if typeof e arg /= ty then
		error "argument type mismatch"
	else ret