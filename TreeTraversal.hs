
-- Defining a tree datatype

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)


dfTraversal :: Tree a -> [a]

-- Depth first traversal

dfTraversal Empty                    = []
dfTraversal (Node root left right)   = root : (dfTraversal left) ++ (dfTraversal right)

-- Breadth first traversal

bfTraversal tree = tbf [tree]
    where
        tbf []                              = []
        tbf xs                              = NodeValue xs ++ (tbf $ concatmap LeftRightNodes xs)
        NodeValue (a _ _ ) = a
        LeftRightNodes (Node _ Empty Empty) = []
        LeftRightNodes (Node _ Empty b)     = [b]
        LeftRightNodes (Node _ a Empty)     = [a]
        LeftRightNodes (Node a b)           = [a, b]




