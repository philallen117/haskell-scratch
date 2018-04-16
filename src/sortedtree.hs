-- sortedtree.hs
-- Jeremy.Singer@glasgow.ac.uk
-- Example code for #FLhaskell course

-- Nodes contain integers, Leaves are empty
data Tree = Leaf | Node Int Tree Tree deriving Show


treeDepth :: Tree -> Int
-- longest path from root to a leaf
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)


isSortedTree :: Tree -> Int -> Int -> Bool
-- is the tree sorted in-order?
-- the two Int params indicate min and max
-- for the current subtree
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted  = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x< maxVal && leftSorted && rightSorted


addNewMax :: Tree -> Tree
-- add a new max element to tree
-- will go down rightmost path to Leaf
addNewMax Leaf = Node 0 Leaf Leaf  -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)  -- this is the rightmost Node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree

insertInOrder :: Int -> Tree -> Tree
-- Input tree must be ordered
insertInOrder i Leaf = Node i Leaf Leaf
insertInOrder i (Node j l r)
    | i < j     = Node j (insertInOrder i l) r
    | otherwise = Node j l (insertInOrder i r)


toList :: Tree -> [Int]
toList Leaf = []
toList (Node i l r) = toList l ++ (i : toList r)


testTree :: Tree
testTree = Node 5 (Node 4 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 7 (Node 6 Leaf Leaf) Leaf)