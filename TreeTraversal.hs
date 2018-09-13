

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)


dfTraversal :: Tree a -> [a]

dfTraversal Empty                    = []
dfTraversal (Node root left right)   = root : (dfTraversal left) ++ (dfTraversal right)
