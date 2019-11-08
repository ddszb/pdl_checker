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
-- showProg (POO op a b)= "(" ++ (showOp op) ++ "(" ++ a ++ "," ++ b  ++ "))" 
-- showProg (POP op a p) = "(" ++ (showOp op) ++ "(" ++ a ++ "," ++ (showProg p) ++ "))" 

prog_E = P SEQ (O "a") (P ND (O "b") (P SEQ (O "c") (P SEQ (O "f") (O "g"))))
-- prog_E = POP SEQ "a" (POP ND "b" (POP SEQ "c" (POO SEQ "f" "g"))) -- (a;b;c;(f U g))
graph_E = [("1","2","b"),("1","3","b"),("1","4","a"),("2","5","e"),("4","5","b"),
			("5","10","c"),("10","11","a"),("10","12","g")]