
-- Функция декодирования двоичной записи числа
-- (аналогично декодированию азбуки Морзе, но
-- учтите бесконечность дерева -- нужна ленивость)
decodeBinary :: String -> Integer
decodeBinary = foldl (\n x -> helper n x) 0
  where helper n '1' = n*2 + 1
        helper n '0' = n*2 

-- Функция декодирования записи числа в системе
-- Фибоначчи: разряды -- числа Фибоначчи, нет
-- двух единиц подряд:
--    0f = 0
--    1f = 1
--   10f = 2
--  100f = 3
--  101f = 4
-- 1000f = 5
-- 1001f = 6
-- 1010f = 7
--   .....
-- (аналогично декодированию азбуки Морзе, но
-- учтите бесконечность дерева -- нужна ленивость)
decodeFibo :: String -> Integer
decodeFibo xs = myFoldr (\prev curr x n -> helper n prev curr x) 0 1 $ reverse xs
  where myFoldr :: (Integer -> Integer -> Char -> Integer -> Integer) -> Integer -> Integer -> [Char] -> Integer
        myFoldr f prev curr [] = 0
        myFoldr f prev curr ('f':xs) = myFoldr f prev curr xs
        myFoldr _ _ _ ('1':'1':_) = error "Incorrect args!"
        myFoldr f prev curr (x:xs) = f prev curr x $ myFoldr f curr (prev + curr) xs
        
        helper n prev curr '1' = n+prev+curr
        helper n prev curr '0' = n

