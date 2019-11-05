data Nat = Zero | Succ Nat

iszero :: Nat -> Bool
iszero Zero = True
iszero (Succ _) = False

pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

soma :: Nat -> Nat -> Nat
soma m Zero = m
soma m (Succ n) = Succ (soma m n)

multi :: Nat -> Nat -> Nat
multi m Zero = Zero
multi m (Succ n) = soma (multi m n) m


-- binary tree
data BTree a = Empty
				| Node (BTree a) a (BTree a)
			deriving (Eq, Show)

size :: BTree a -> Int
size Empty = 0
size (Node tl x tr) = size tl + 1 + size tr

height :: BTree a -> Int
height Empty = 0
height (Node tl x tr) = 1 + max (height tl) (height tr)

reflect :: BTree a -> BTree a
reflect Empty = Empty
reflect (Node tl x tr) = Node (reflect tr) x (reflect tl)


levels t = concat (myLevels t)

join :: [[a]] -> [[a]] -> [[a]]
join [] yss = yss
join xss [] = xss
join (xs:xss) (ys:yss) = (xs ++ ys): join xss yss

myLevels :: BTree a -> [[a]]
myLevels Empty = []
myLevels (Node t1 x t2) = [x]: join (myLevels t1) (myLevels t2)

-- grafo
-- data Grafo a = Empty | Node ()

tree = Node (Node Empty 4 Empty) 6
	(Node (Node Empty 2 Empty) 3
		(Node Empty 5 Empty))



-- data Grafo = Empty 
-- 				| Nos [Aresta]


