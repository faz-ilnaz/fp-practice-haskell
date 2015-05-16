

{- Нахождение максимума в списке -}
myMaximum :: [Int] -> Int
myMaximum = foldr1(\a b -> if a > b then a else b)

-- | Нахождение максимум и его индекс 
-- >>> myMaxIndex [1,5,2,3,4]
-- (1,5)
myMaxIndex :: [Integer] -> (Int,Integer)
myMaxIndex list = 
  let z :: [Integer] -> Int -> (Int, Integer)
      z [] _ = error "list is empty"
      z [x] xi = (xi, x)
      z (x:xs) xi
        | x > maxValue  = (xi,x)
        | otherwise = (indexOfMax, maxValue)
        where (indexOfMax, maxValue) = z xs (xi +1)
  in z list 0

-- | Количество элементов, равных максимальному
-- >>> maxCount [1,5,3,10,3,10,5]
-- 2
maxCount :: [Integer] -> Int
maxCount [] = 0
maxCount (x:xs) =
  let z :: [Integer] -> Integer -> Int -> Int
      z [] _ maxC = maxC
      z (x:xs) maxV maxC 
        | x >  maxV = z xs x 1
        | x == maxV = z xs maxV (maxC+1)
        | otherwise = z xs maxV maxC
  in z xs x 1 

-- | Количество элементов между минимальным и максимальным
-- >>> countBetween [-1,3,100,3]
-- 2
--
-- >>> countBetween [100,3,-1,3]
-- -2
--
-- >>> countBetween [-1,100]
-- 1
--
-- >>> countBetween [1]
-- 0

{-
  minV - min value
  maxV - max value
  minI1 - first index of min elem
  minI2 - last index of min elem
  maxI1 - first index of max elem
  mazI2 - last index of max elem
  n - counter
-}
countBetween :: [Integer] -> Int
countBetween [] = 0
countBetween (x:xs) = 
  let z :: [Integer] -> Integer -> Integer -> Int -> Int -> Int -> Int -> Int -> Int
      z [] minV maxV minI1 minI2 maxI1 maxI2 n
        | abs(maxI1 - minI1) < abs(maxI2 - minI2) = maxI1 - minI1
        | otherwise = maxI2 - minI2
      z (x:xs) minV maxV minI1 minI2 maxI1 maxI2 n
        | x > maxV  = if(abs(minI2-n) < abs(minI1-n)) then z xs minV x minI2 minI2 n n (n+1) else z xs minV x minI1 minI2 n n (n+1) 
        | x < minV  = if(abs(n-maxI2) < abs(n-maxI1)) then z xs x maxV n n maxI2 maxI2 (n+1) else z xs x maxV n n maxI1 maxI2 (n+1)
        | x == minV && x == maxV = z xs minV maxV minI1 n maxI1 n (n+1)
        | x == minV = if(abs(n-maxI2) < abs(minI1-maxI2)) then z xs minV maxV n n maxI1 maxI2 (n+1) else z xs minV maxV minI1 n maxI1 maxI2 (n+1)
        | x == maxV = if(abs(minI2-n) < abs(minI2-maxI1)) then z xs minV maxV minI1 minI2 n n (n+1) else z xs minV maxV minI1 minI2 maxI1 n (n+1)
        | otherwise = z xs minV maxV minI1 minI2 maxI1 maxI2 (n+1)

  in z xs x x 0 0 0 0 1