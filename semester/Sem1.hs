data Term = Var String -- variable
          | App Term Term -- application
          | Lam String Term -- lambda abstraction

instance Show Term where
    show (Lam argname body) =
        "(lambda " ++ argname ++ ". " ++ show body ++ ")"
    show (App function argument) =
        "(" ++ show function ++ " " ++ show argument ++ ")"
    show (Var name) = name

type Env = [(String, Term)]

eval :: Env -> Term -> Term
eval env e@(Var a) = maybe e id (lookup a env)
eval env (Lam x e) = Lam x (eval env e)
eval env (App e1 e2) = apply env (eval env e1) (eval env e2)

apply :: Env -> Term -> Term -> Term
apply env (Lam x e) e2 = eval ((x, e2):env) e
apply env e1 e2 = App e1 e2


true  = Lam "t" (Lam "f" (Var "t"))
false = Lam "t" (Lam "f" (Var "f"))



suc = Lam "n" (Lam "f" (Lam "x" 
           (App (Var "f") 
                (App (App (Var "n") (Var "f"))
                     (Var "x")))))

num0  = Lam "f" (Lam "x" (Var "x"))
num1  = App suc num0
num2  = App suc num1
num3  = App suc num2
num4  = App suc num3
num5  = App suc num4
num6  = App suc num5
num7  = App suc num6
num8  = App suc num7
num9  = App suc num8
num10 = App suc num9

isZero = Lam "n" (Lam "x" (Lam "y" 
            (App (App (Var "n") (Lam "z" (Var "y"))) (Var "x"))))

add = Lam "m" (Lam "n" (Lam "f" (Lam "x"
           (App (App (Var "m") (Var "f"))
                (App (App (Var "n") (Var "f"))
                     (Var "x"))))))
mul = Lam "m" (Lam "n" (Lam "f"
           (App (Var "m") (App (Var "n") (Var "f")))))

ex34  = App (App add num3) num4 -- = 7


test = App (Lam "x" (Lam "y" (Var "x"))) (Lam "z" (Var "z"))