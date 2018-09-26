
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
        tbf xs                              = map NodeValue xs ++ (tbf $ concatmap LeftRightNodes xs)
        NodeValue (v _ _ )                  = v
        LeftRightNodes (Node _ Empty Empty) = []
        LeftRightNodes (Node _ Empty b)     = [b]
        LeftRightNodes (Node _ a Empty)     = [a]
        LeftRightNodes (Node a b)           = [a, b]




-- In order tree traversal

inOrderTraversal :: Tree a -> [a]
inOrderTraversal Empty                               = []
inOrderTraversal Node a l r                          = (ino l) ++ [a] ++ (ino r)


-- Pre order tree traversal


preOrderTraversal :: Tree a -> [a]
preOrderTraversal Empty                              = []
preOrderTraversal Node a l r                         = [a] ++ (preOrderTraversal l) ++ (preOrderTraversal r)
