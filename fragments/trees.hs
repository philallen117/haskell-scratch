data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

-- sumTree :: Num a => Tree a -> a
-- sumTree Leaf = 0
-- sumTree (Node i l r) = i + sumTree l + sumTree r

sumTree :: Monoid a => Tree a -> a
sumTree Leaf = mempty
sumTree (Node i l r) = i `mappend` sumTree l `mappend` sumTree r
