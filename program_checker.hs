type Operand = [Char] 
type Edge = [Char]  
type Vertice = [Char] 
type Node = (Vertice, Vertice, Edge) 

data Operator = SEQ | ND | FR 
		deriving (Eq, Show)

data Program p = P Operator (Program p) (Program p)| O Operand | Empty

type Graph = [Node]

-- Uma representação melhor do grafo [uso: putStrLn (drawGraph var) ]
showGraph :: Graph -> String
showGraph ((a,b,c):gr) = a ++ "--" ++ c ++ "-->" ++ b ++ "\n" ++ showGraph gr
showGraph [] = ""

		 -- deriving (Eq,Show)

showOp :: Operator -> String
showOp SEQ = ";"
showOp ND = "U"
showOp FR = "*"

-- Uma representação melhor do programa [uso: putStrLn (drawProg var) ]
showProg :: Program p -> String
showProg (O x) = x
showProg Empty = "" 
showProg (P op a b) = "(" ++ (showOp op) ++ "(" ++ (showProg a) ++ "," ++ (showProg b) ++ "))" 

-- a ; ( b U (c;f;g))
prog_E = P SEQ (O "a") (P ND (O "b") (P SEQ (O "c") (P SEQ (O "f") (O "g"))))

graph_E = [("1","2","b"),("1","3","b"),("1","4","a"),("2","5","e"),("4","5","b"),
			("5","10","c"),("10","11","a"),("10","12","g")]


validate :: Program p -> Graph -> IO ()
validate a b = putStrLn ("Caminho percorrido: \n" ++ check a "1" b b )

check :: Program p -> [Char] -> Graph -> Graph -> [Char]
check (P _ (O a) _ ) _ x [] = "\nFalse:  expected " ++ "x--" ++ a ++ "-->y but the graph is over!\n" 
check (P op (O f) (O [])) node ((a,b,c):gr) _ = "False"

check (P op (O f) p) node ograph ((a, b, c):gr) =
	if a /= node || f /= c
		then check (P op (O f) p) node ograph gr
	else do
		if op == SEQ
			then do
				let newEdge = (node, b, f)
				if newEdge == (a, b, c)
					then showGraph [(a,b,c)] ++ check p b ograph ograph
				else check (P op (O f) p) node ograph gr
		else do
			if op == ND
				then do
					let newEdge = (node, b, f)
					if newEdge == (a, b, c)
						then showGraph [(a,b,c)] ++ "True. No need to check " ++ showProg p
					else do "dois " ++ check p node ograph ograph
			else "Nope"

