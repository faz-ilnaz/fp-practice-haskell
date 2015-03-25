data Tree a = Empty
            | Branch (Tree a) a (Tree a)
              deriving (Show)

tr1 :: Tree Int
tr1 = Branch Empty 4 (Branch (Branch Empty 3 Empty) 2 (Branch (Branch Empty 1 Empty) 5 Empty))




tr2 :: Tree Int
tr2 = Branch Empty 4 (Branch Empty 3 Empty)


treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Branch x _ y) = 1 + max (treeHeight x) (treeHeight y)

tmap :: Tree a -> (a -> b) -> b
tmap Empty f = 0
tmap (Branch x a y) f = tmap(Branch( x (f a) y)) f 