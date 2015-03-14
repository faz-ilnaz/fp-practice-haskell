-- | deletes first arg from second arg
-- >>> deleteInt 1 [1,2,1,3,4]
-- [2,3,4]
--
-- >>> deleteInt 1 [2,3]
-- [2,3]
--
-- >>> deleteInt 1 []
-- []
deleteInt :: Int -> [Int] -> [Int]
deleteInt x [] = []
deleteInt x (y:ys)  | x==y      = deleteInt x ys
                    | otherwise = y : deleteInt x ys
             
-- | returns list of indices of first arg in second arg
-- >>> findIndicesInt 1 [1,2,1,3,4]
-- [0,2]
--
-- >>> findIndicesInt 1 [2,3]
-- []
findIndicesInt :: Int -> [Int] -> [Int]
findIndicesInt x [] = []
findIndicesInt x ys = withIndexCounter
    where withIndexCounter = findIndicesHelper 0 x ys

          findIndicesHelper :: Int -> Int -> [Int] -> [Int]
          findIndicesHelper n x [] = []
          findIndicesHelper n x (y:ys)  | (x==y) = n : findIndicesHelper (n+1) x ys
                                        | otherwise = findIndicesHelper (n+1) x ys