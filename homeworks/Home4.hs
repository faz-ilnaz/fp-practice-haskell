data Expr = Val Int
          | Plus Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          	deriving(Show)

eval :: Expr -> Int
eval (Val x) = x
eval (Plus e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Mult e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = if (eval e2 == 0) then error "division by zero detected!"
										else (eval e1 `div` eval e2)
eval (Mod e1 e2) = (eval e1)`mod`(eval e2)