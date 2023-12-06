-- Binary Tree of generic type that can either be empty, or can contain an element and two other trees


-- For the epxression tree I believe:
-- 'a' would be the op
-- other trees would be left and right branch respectively

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- let myTree = foldl treeInsert EmptyTree [a]

-- tree with just one node, 
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => Tree a -> a -> Tree a
treeInsert EmptyTree x = singleton x
treeInsert (Node a left right) x
    | x == a = Node x left right
    | x < a = Node a (treeInsert left x) right 
    | x > a = Node a left (treeInsert right x)

-- Node a (treeInsert left x) right   -> inserts left
-- Node a left (treeInsert right x)   -> inserts right

treeElem :: (Ord a) => Tree a -> a -> Bool
treeElem EmptyTree x = False
treeElem (Node a left right) x
   | x == a = True
   | x < a = treeElem left x
   | x > a = treeElem right x


operators = ["∅", "ε", "|", "+", "*", "?", "@"]


isOperator :: (Eq a) => a -> [a] -> Bool
isOperator a list = a `elem` list