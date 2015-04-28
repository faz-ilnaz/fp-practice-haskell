-- арифметические прогрессии длины 3 с расстоянием k
-- например, take 2 (arith 2) = [[5,11,17], [7,13,19]]
arith :: Int -> [[Int]]
arith k = filter isK $ zipW3 (\x y z -> [x,y,z]) primes (offset k primes) (offset (2*k) primes)

offset :: Int -> [Int] -> [Int]
offset 0 xs 		= xs
offset n (x:xs) = offset (n-1) xs

isK :: [Int] -> Bool
isK (a:b:c:xs) = b-a == c-b

-- zipWith
zipW3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipW3 f [] [] [] = []
zipW3 f (a:as) (b:bs) (c:cs) = f a b c : zipW3 f as bs cs

nat = [1..]

isPrime 1 = False
isPrime 2 = True
isPrime n | even n = False
          | otherwise = null $ filter (\i -> n`mod`i == 0) $ takeWhile (\i -> i*i <=n) [3,5..]

primes = filter isPrime nat

-- положение ферзей на шахматной доске
-- список номеров горизонталей, на которых находятся ферзи
-- например, Board [1,2,3,4,5,6,8,7] -- это такое расположение
--  +--------+
-- 8|      ♕ |
-- 7|       ♕|
-- 6|     ♕  |
-- 5|    ♕   |
-- 4|   ♕    |
-- 3|  ♕     |
-- 2| ♕      |
-- 1|♕       |
-- -+--------+
--  |abcdefgh|

newtype Board = Board { unBoard :: [Int] } deriving (Eq,Show)

queens :: Int -> [Board]
queens n = undefined

-- Белые начинают и дают мат в два хода
-- 
-- Белые: Кd8 Kh8 Сf7 Крf6
-- Черные: Крf8 Кe7 Kg7
-- 
-- (написать перебор всех вариантов полуходов длины три,
-- вернуть список последовательностей полуходов, ведущих
-- к решению; до этого определить необходимые типы)
-- См. https://en.wikipedia.org/wiki/Chess_notation

-- solutions :: [[Move]]

