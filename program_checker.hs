import Data.List

type Operand = [Char] 
type Edge = [Char]  
type Vertice = [Char] 
type Node = (Vertice, Vertice, Edge) 

data Operator = SEQ | ND | FR deriving (Eq, Show)

data Program p = P Operator (Program p) (Program p)| O Operand

type Graph = [Node]

main = do
	validate kprog kgraph

-- (a;b) U (b;c)
kprog = P ND  (P SEQ (O "a") (O "b")) (P SEQ (O "b") (O "c"))  
kgraph = [("1","2","a"),("1","5","c"),("1","8","a"),("2","3","f"),("5","6","c"), ("8", "9", "j")] 


validate :: Program p -> Graph -> IO ()
validate a b = putStrLn ("\n\n  Program: \n\n    " ++ showProg a ++
		 "\n\n  Graph:\n" ++ showGraph b ++ "\n\n  Evaluation: \n\n  " 
		 ++ check a (head b) b b ++ "\n\n")


check :: Program p -> Node -> Graph -> Graph -> [Char]
check (P op (O f) p ) node [] [] = "\nFalse:  expected " ++ (getIndex node) ++ "--" ++ f ++ "-->x but the graph is over!\n" 
check (P op p (O f)) node [] [] = "False."

check (P op x y ) (a,b,c) ograph [] = 
	do 
	if 	c /= "0"
		then do
			let newGraph =  removeMatch (a,b,c ) ograph
			if newGraph == [] 
				then "False"
			else check (P op x y ) (a, "x", "0") newGraph newGraph
	else do
	let previous = getFromMatch (b,a,c) ograph
	let newGraph = removeMatch (b,a,c) ograph
	let prevCheck = check (P op ( O previous) (O "END_OF_PROGRAM")) (head newGraph) newGraph newGraph
	if lookFor "False" prevCheck == Nothing
		then check (P op x y) ([last prevCheck], "x", "0") newGraph newGraph
	else "False"

check (P op (O f) (O g)) node ograph ((a,b,c):gr) =
	if op == SEQ 
		then do
			let index = getIndex node
			let newEdge = (index,b,f)
			if newEdge == (a,b,c)
				then do
					if g == "END_OF_PROGRAM"
						then "True. Final node was " ++ b
					else do
						let end = "END_OF_PROGRAM"
						let newNode = (b,a,"0")
						check (P op (O g) (O end)) newNode ograph ograph
			else check (P op (O f) (O g)) node ograph gr
	else do
		if op == ND
			then do 
				let index = getIndex node
				let leftEdge = (index, b, f)
				if leftEdge == (a,b,c) -- If it goes through on the left side, no need to check right
					then "True. Final node was " ++ b 
				else do -- If it doesn't go through left, check again through the graph.
					let restOfLeft = check (P op (O f) (O g)) node ograph gr
					if lookFor "False" restOfLeft == Nothing -- If the rest goes through, than it's true
						then "True. Final node was " ++ ([last restOfLeft])
					else do	-- If left failed, try right			
						if g == "END_OF_PROGRAM"
							then "False."
						else do 
							let end = "END_OF_PROGRAM"
							check (P op (O g) (O end)) node ograph ograph
		else "* nao implementado"

check (P op (O f) p ) node ograph ((a,b,c):gr) =
	if op == SEQ
		then do 
			let index = getIndex node
			let newEdge = (index,b,f)
			if newEdge == (a,b,c)
				then do
					let newNode = (b, a, "0")
				 	check p newNode ograph ograph
			else check (P op (O f) p) node ograph gr
	else do
		if op == ND
			then do 
				let index = getIndex node
				let newEdge = (index, b, f)
				if newEdge == (a,b,c)
					then "True. Final node was " ++ b
				else do 
					let restOfLeft = check (P op (O f) p) node ograph gr
					if lookFor "False" restOfLeft == Nothing
						then "True. Final node was " ++ [last restOfLeft] 
					else do
						let end = "END_OF_PROGRAM"
						check (P op p (O end)) node ograph ograph
		else "* nao implementado"

check (P op p q) node ograph ((a,b,c):gr) =
	if op == SEQ
		then do
			let leftEdge = check p node ograph ((a,b,c):gr)
			if lookFor "False" leftEdge == Nothing
				then do
					let end = "END_OF_PROGRAM"
					let pos = [last leftEdge]
					check (P op q (O end)) (pos, "0", "0")  ograph ograph
			else "False."
	else do
		if op == ND
			then do 
				let leftEdge = check p node ograph ((a,b,c):gr)
				if lookFor "False" leftEdge == Nothing
					then "True. Final node was " ++ [last leftEdge]
				else do
					let end = "END_OF_PROGRAM"
					check (P op q (O end)) node ograph ograph
		else "* nao implementado"

getFromMatch :: Node -> Graph -> [Char]
getFromMatch _ [] = []
getFromMatch (a,b,c) ((x,y,s):xs)
					| a == x  && b == y = s
					| otherwise = getFromMatch (a,b,c) xs

removeMatch :: Node -> Graph -> Graph
removeMatch _ [] = []
removeMatch (a,b,c) ((x,y,s):xs)
					| a == x && b == y = xs 
					| otherwise = (x,y,s): removeMatch (a,b,c) xs
                    

getIndex :: Node -> [Char]
getIndex (a, _, _ ) = a

lookFor :: (Eq a) => [a] -> [a] -> Maybe Int
lookFor search str = findIndex (isPrefixOf search) (tails str)


showGraph :: Graph -> String
showGraph ((a,b,c):gr) = "\n    " ++ a ++ "\t------>\t" ++ b ++ " =\t" ++ c ++  showGraph gr
showGraph [] = ""

showOp :: Operator -> String
showOp SEQ = ";"
showOp ND = " U "
showOp FR = "*"

showProg :: Program p -> String
showProg (O x) = x
showProg (P op a b) = 
	if op /= FR 
		then "(" ++ (showProg a) ++ (showOp op) ++ (showProg b) ++ ")" 
	else do
		"(" ++ (showProg a) ++ ";" ++ showProg b ++ ")*"
