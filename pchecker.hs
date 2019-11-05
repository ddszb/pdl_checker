type Operand = [Char]
type Weight = [Char]
type Vertice = [Char]
type Aresta = (Vertice, Vertice, Weight)

data Operator = SEQ | ND | FR 
		deriving (Eq, Show)

data Program p = PP Operator Operand (Program p) | P1 Operator Operand Operand
			deriving (Eq, Show)

data Graph = Nodes [Aresta]
		 -- deriving (Eq,Show)

-- show :: Graph g -> IO ()
instance Show Graph  where
show (Nodes gr) = putStrLn (drawGraph (Nodes gr))

drawGraph :: Graph -> String
drawGraph (Nodes ((a, b, c):gr)) = a ++ "--" ++ c ++ "-->" ++ b ++ "\n" ++ drawGraph (Nodes gr)
drawGraph (Nodes []) = ""

drawOp :: Operator -> String
drawOp SEQ = ";"
drawOp ND = "U"
drawOp FR = "*"

drawProg :: Program p -> String
drawProg (P1 op a b)= "(" ++ (drawOp op) ++ "(" ++ a ++ "," ++ b  ++ "))" 
drawProg (PP op a p) = "(" ++ (drawOp op) ++ "(" ++ a ++ "," ++ (drawProg p) ++ "))" 


--------------------------------------------------------------
graph1 = Nodes [("1","2", "a"), ("1","3", "b"), ("1","4", "b"), ("1","5", "d"),
				("2","6", "a"), ("2","7", "a"), ("4","8", "d"), ("5","9", "c")]

prog1 = P1 SEQ "a" "b" -- (a;b)
prog2 = PP SEQ "a" (PP SEQ "a" (PP ND "b" (P1 FR "d" "a"))) -- a;(a;(b U (d;a)*))
seqprog1 = PP SEQ "a" (PP SEQ "b" (PP SEQ "c" (P1 SEQ "d" "e"))) -- (a;b)


getCurr :: Program p -> Program p 
getCurr (PP op a (PP op2 b prog)) =
	if op == SEQ
		then do
			if op2 == SEQ
				then (P1 op a b)
				else (P1 op a a)
		else (P1 op b b)		