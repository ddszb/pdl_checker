type Operand = [Char] 
type Weight = [Char]  
type Vertice = [Char] 
type Aresta = (Vertice, Vertice, Weight) 

data Operator = SEQ | ND | FR 
		deriving (Eq, Show)

data Program p = POP Operator Operand (Program p) | POO Operator Operand Operand | PPP Operator (Program p) (Program p) | PPO Operator (Program p) Operand 
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
drawProg (POO op a b)= "(" ++ (drawOp op) ++ "(" ++ a ++ "," ++ b  ++ "))" 
drawProg (POP op a p) = "(" ++ (drawOp op) ++ "(" ++ a ++ "," ++ (drawProg p) ++ "))" 



graph2 = [("1","2", "a"), ("1","3", "b"), ("1","4", "b"), ("1","5", "d"),
				("2","6", "a"), ("2","7", "a"), ("4","8", "d"), ("5","9", "c")]

prog1 = POO SEQ "b" "c" -- (a;b)
prog2 = POP SEQ "a" (POP SEQ "a" (POP ND "b" (POO FR "d" "a"))) -- a;(a;(b U (d;a)*))
seqprog1 = POP SEQ "b" (POP SEQ "b" (POP SEQ "c" (POO SEQ "d" "e"))) -- (b;b;c;d;e)

seq_A = POP SEQ "b" (POP SEQ "b" (POP SEQ "c" (POO SEQ "d" "e"))) -- (b;b;c;d;e)
graph_A = [("1","2","b"),("2","3","b"),("3","4","c"),("4","5","e")]

check :: Program p -> [Char] -> Graph -> String

check _ _ [] = "False" 
check (POO op f []) node ((a,b,c):gr) = "False"
	-- if a /= node 
	-- 	then verify (POO op f "") node gr
	-- else do
	-- 	if op == SEQ 
	-- 		then do
	-- 			let newEdge = (node, b, f)
	-- 			if newEdge == (a, b, c)
	-- 				then True
	-- 			else verify (POO op f "") node gr
	-- 	else False

check (POO op f g) node ((a,b,c):gr) =
	-- if a /= node 
	-- 	then check (POO op f g) node gr
	-- else do
		if g == "END_OF_PROGRAM"
			then "False"
		else 
			if op == SEQ 
				then do
					let newEdge = (node, b, f)
					if newEdge == (a, b, c)
						then do
							let end = "END_OF_PROGRAM"
							check (POO op g end) b gr
					else "False"
			else "False"

verify :: Program p -> [Char] -> Graph -> Bool
verify (POP op f p) index ((a,b,c):gr) = 
	if op == SEQ
		then do 
			let newEdge = (index, b, f)
			if newEdge == (a, b, c)
				then verify p b gr 
			else False
	else  
		if op == ND 
			then do
				let leftEdge = (index, b, f)
				-- let (op1, r, s = p --< como ?
				let rightEdge = (index,b, f)
				if leftEdge == (a, b, c)
					then verify p b gr
				else 
					if rightEdge == (a, b, c)
						then verify p b gr
					else False
		else False

verify (POO op f g) index ((a,b,c):gr) =
	if op == SEQ
		then do 
			let newEdge = (index, b, f)
			if newEdge == (a, b, c)
				then True
			else False
		else False 

getCurr :: Program p -> Program p 
getCurr (POP op a (POP op2 b prog)) =
	if op == SEQ
		then do
			if op2 == SEQ
				then (POO op a b)
				else (POO op a a)
		else (POO op b b)		