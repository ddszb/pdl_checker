type Operand = [Char] 
type Weight = [Char]  
type Vertice = [Char] 
type Aresta = (Vertice, Vertice, Weight) 

data Operator = SEQ | ND | FR 
		deriving (Eq, Show)

data Program p = PP Operator Operand (Program p) | P1 Operator Operand Operand
			deriving (Eq, Show)

type Graph = [Aresta]


drawGraph :: Graph -> String
drawGraph ((a,b,c):gr) = a ++ "--" ++ c ++ "-->" ++ b ++ "\n" ++ drawGraph gr
drawGraph [] = ""

		 -- deriving (Eq,Show)

drawOp :: Operator -> String
drawOp SEQ = ";"
drawOp ND = "U"
drawOp FR = "*"

drawProg :: Program p -> String
drawProg (P1 op a b)= "(" ++ (drawOp op) ++ "(" ++ a ++ "," ++ b  ++ "))" 
drawProg (PP op a p) = "(" ++ (drawOp op) ++ "(" ++ a ++ "," ++ (drawProg p) ++ "))" 



graph2 = [("1","2", "a"), ("1","3", "b"), ("1","4", "b"), ("1","5", "d"),
				("2","6", "a"), ("2","7", "a"), ("4","8", "d"), ("5","9", "c")]

prog1 = P1 SEQ "b" "c" -- (a;b)
prog2 = PP SEQ "a" (PP SEQ "a" (PP ND "b" (P1 FR "d" "a"))) -- a;(a;(b U (d;a)*))
seqprog1 = PP SEQ "b" (PP SEQ "b" (PP SEQ "c" (P1 SEQ "d" "e"))) -- (b;b;c;d;e)

seq_A = PP SEQ "b" (PP SEQ "b" (PP SEQ "c" (P1 SEQ "d" "e"))) -- (b;b;c;d;e)
graph_A = [("1","2","b"),("2","3","b"),("3","4","c"),("4","5","e")]

verify :: Program p -> [Char] -> Graph -> Bool
verify (PP op f p) index ((a,b,c):gr) = 
	if op == SEQ
		then do 
			let newEdge = (index, b, f)
			if newEdge == (a, b, c)
				then verify p b gr 
			else False
	else  False

verify (P1 op f g) index ((a,b,c):gr) =
	if op == SEQ
		then do 
			let newEdge = (index, b, f)
			if newEdge == (a, b, c)
				then True
			else False
		else False 

getCurr :: Program p -> Program p 
getCurr (PP op a (PP op2 b prog)) =
	if op == SEQ
		then do
			if op2 == SEQ
				then (P1 op a b)
				else (P1 op a a)
		else (P1 op b b)		