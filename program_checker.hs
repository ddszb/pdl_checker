type Operand = [Char] 
type Edge = [Char]  
type Vertice = [Char] 
type Node = (Vertice, Vertice, Edge) 

data Operator = SEQ | ND | FR deriving (Eq, Show)

data Program p = P Operator (Program p) (Program p)| O Operand | Empty

type Graph = [Node]


-- PROGRAM AND GRAPH INPUT BELOW

-- (e U (a; b; (c U d))
lprog = P ND (O "e") (P SEQ (O "a") (P SEQ (O "b") (P ND (O "c") (O "d"))))
lgraph = [("1","2","a"), ("2","3","b"),("3","4","c")]

-----------------------------------------------------


-- Uma representação melhor do grafo [uso: putStrLn (drawGraph var) ]
showGraph :: Graph -> String
showGraph ((a,b,c):gr) = a ++ "--" ++ c ++ "-->" ++ b ++ "\n" ++ showGraph gr
showGraph [] = ""

		 -- deriving (Eq,Show)

showOp :: Operator -> String
showOp SEQ = ";"
showOp ND = " U "
showOp FR = "*"

-- Uma representação melhor do programa [uso: putStrLn (drawProg var) ]
showProg :: Program p -> String
showProg (O x) = x
showProg Empty = "" 
showProg (P op a b) = "(" ++ (showProg a) ++ (showOp op) ++ (showProg b) ++ ")" 

validate :: Program p -> Graph -> IO ()
validate a b = putStrLn ("Programa: \n\n" ++ showProg a ++ "\n\nGrafo\n" ++ showGraph b ++ "\n\nCaminho percorrido: \n" ++ check a "1" (head b) b b )


-- (a;b;c;d;e)
kprog = P SEQ (O "a") (P SEQ (O "b") (P SEQ (O "c") (P SEQ (O "d") (O "e"))))
kgraph = [("1","7","a"),("1","4","a"),("1","2","a"),("3","4","c"),("2","3","b"),("5","6","e"),("1","3","b"),("2","3","d"),("2","3","c"),
			("3","4","d"),("4","5","d")]


check :: Program p -> [Char] -> Node -> Graph -> Graph -> [Char]
check (P op (O f) p ) index node [] [] = "\nFalse:  expected " ++ index ++ "--" ++ f ++ "-->x but the graph is over!\n" 
check (P op (O f) p ) index node ograph [] =
	do 
	let newGraph =  removeFrom ograph node
	"[R1] : [X]\n\n" ++ showGraph ([head newGraph])  ++ check (P op (O f) p ) (getIndex(head newGraph)) (head newGraph) newGraph newGraph


check (P op (O f) (O [])) index node ((a,b,c):gr) _ = "False"

check (P op (O f) (O g)) index node ograph ((a,b,c):gr) =
	-- if a /= index || f /= c 
	-- 	then do "[P0]" ++  check (P op (O f) (O g)) index ograph gr
	-- else do
		if op == SEQ
			then do
				let newEdge = (index, b, f)
				if newEdge == (a,b,c)
					then do 
						if g == "END_OF_PROGRAM"
							then "[P1]" ++ showGraph [(a,b,c)] ++ "True."
						else do
							let end = "END_OF_PROGRAM"
							"[P2]" ++ showGraph [(a,b,c)] ++ check (P op (O g) (O end)) b newEdge ograph ograph
				else "[P3]" ++  check (P op (O f) (O g)) index node ograph gr
		else do
			if op == ND -- ("3","4","c")]
				then do
					if g == "END_OF_PROGRAM"
						then do
							let newEdge = (index, b, f)
							if newEdge == (a,b,c)
								then "[P4]" ++ showGraph [(a,b,c)] ++ "True2"
							else "[P5]" ++ check (P op (O f) (O g)) index node ograph gr
					else do 
						let newEdge = (index, b, f)
						if newEdge == (a, b, c)
							then "[P6]" ++ showGraph [(a,b,c)] ++ "True3"
						else do
							-- "[P7]" ++ check (P op (O f) (O g)) index node ograph gr
							let end = "END_OF_PROGRAM"
							"[P8]" ++ check (P op (O g) (O end)) index node ograph ograph 
			else "WIP"

check (P op (O f) p) index node ograph ((a, b, c):gr) =
	-- if a /= index || f /= c
	-- 	then "[PF1]" ++ check (P op (O f) p) index ograph gr
	-- else do
		if op == SEQ
			then do
				let newEdge = (index, b, f)
				if newEdge == (a, b, c)
					then "[P9]" ++ showGraph [(a,b,c)] ++ check p b newEdge ograph ograph
				else "[P10]" ++ check (P op (O f) p) index node ograph gr
		else do
			if op == ND
				then do
					let newEdge = (index, b, f)
					if newEdge == (a, b, c)
						then "[P11]" ++  showGraph [(a,b,c)] ++ "True. No need to check " ++ showProg p
					else do "[P12]" ++ check p index node ograph ograph
			else "Nope"


getIndex :: Node -> [Char]
getIndex (a, _, _ ) = a

moveToEnd :: Node -> Graph -> Graph
moveToEnd node graph =
	do
		let x = removeFrom graph node
		addTo x node

removeFrom ::  Graph -> Node -> Graph
removeFrom [] _ = []
removeFrom (y:ys) x | x == y    = removeFrom ys x
                    | otherwise = y : removeFrom ys x

addTo :: Graph -> Node -> Graph
addTo [] a = [a]
addTo (x:xs)  a  = x : addTo xs a