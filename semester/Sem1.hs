import Data.List

data Term = Var String -- variable
          | App Term Term -- application
          | Lam String Term -- lambda abstraction
          deriving Eq

instance Show Term where
    show (Lam argname body) =
        "(\\" ++ argname ++ ". " ++ show body ++ ")"
    show (App function argument) =
        "(" ++ show function ++ " " ++ show argument ++ ")"
    show (Var name) = name

eval :: Term -> Term
eval exp = calc exp []
  where calc (App f a) as = calc f (a:as)
        calc (Lam s e) [] = Lam s (eval e)
        calc (Lam s e) (a:as) = calc (subst s a e) as
        calc f as = app f as
        app f as = foldl App f (map eval as)


subst :: String -> Term -> Term -> Term
subst var to in1 = mySubst in1
  where mySubst exp@(Var i) = if i == var then to else exp
        mySubst (App f a) = App (mySubst f) (mySubst a)
        mySubst (Lam i exp) =
            if var == i then
                Lam i exp
            else if i `elem` freeVars then
                let i' = cloneSym exp i
                    exp' = substVar i i' exp
                in  Lam i' (mySubst exp')
            else
                Lam i (mySubst exp)
        freeVars = freeV to
        cloneSym exp i = loop i
           where loop i' = if i' `elem` vars then loop (i ++ "'") else i'
                 vars = freeVars ++ freeV exp

substVar :: String -> String -> Term -> Term
substVar s s' exp = subst s (Var s') exp

freeV :: Term -> [String]
freeV (Var s) = [s]
freeV (App f a) = freeV f `union` freeV a
freeV (Lam i e) = delete i $ freeV e




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


-------------------------
-----   TESTS ------------
---------------------------

-- 1
-- Input: ((λ x. x) (λ y. (λ z. z)))
-- Output: (λ y. (λ z. z))
test1 = App(Lam "x" (Var "x")) (Lam "y" (Lam "z" (Var "z")))
test1A = Lam "y" (Lam "z" (Var "z"))
assert1 = eval test1 == test1A


-- 2
-- Input: (λ x. ((λ y. y) x))
-- Output: (λ x. x)
test2 = Lam "x" (App (Lam "y" (Var "y")) (Var "x") )
test2A = Lam "x" (Var "x")
assert2 = eval test2 == test2A

-- 3
-- Input: ((λ x. (λ y. x)) (λ a. a))
-- Output: (λ y. (λ a. a))
test3 = App (Lam "x" (Lam "y" (Var "x"))) (Lam "a" (Var "a"))
test3A = Lam "y" (Lam "a" (Var "a"))
assert3 = eval test3 == test3A

-- 4
-- Input: (((λ x. (λ y. x)) (λ a. a)) ((λx. (x x)) (λx. (x x))))
-- Output: (λ a. a) 
test4 = App (App (Lam "x" (Lam "y" (Var "x"))) (Lam "a" (Var "a"))) (App (Lam "x" (App (Var "x") (Var "x"))) (Lam "x" (App (Var "x") (Var "x"))))
test4A = Lam "a" (Var "a")
assert4 = eval test4 == test4A

-- 5
-- Input: (λx y.x) y
-- Output: (λy'. y)
test5 = App (Lam "x" (Lam "y" (Var "x"))) (Var "y")
test5A = Lam "y'" (Var "y")
assert5 = eval test5 == test5A

-- 6
-- Input: (λb. λa. a) (λ z. z)
-- Output: (λa. a)
test6 = App (Lam "b" (Lam "a" (Var "a"))) (Lam "z" (Var "z"))
test6A = Lam "a" (Var "a")
assert6 = eval test6 == test6A

-- 7( with numbers)
assert7 = eval ex34 == eval num7