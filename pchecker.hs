type Operand = [Char] 
type Edge = [Char]  
type Vertice = [Char] 
type Node = (Vertice, Vertice, Edge) 

data Operator = SEQ | ND | FR 
		deriving (Eq, Show)

-- O programa prefixado pode ser do tipo:
-- POP - Programa Operando-Programa  ex.: (c; (a U b))
-- PPO - Programa Programa-Operando  ex.: ((a U b); c) 
-- PPP - Programa Programa-Programa  ex.: ((a U b);(c U a) 
-- POO - Programa Operador-Operador  ex.: (a;b)  	/-> um programa de uma operação ou a última operação de qualquer programa )
data Program p = POP Operator Operand (Program p) | POO Operator Operand Operand | PPP Operator (Program p) (Program p) | PPO Operator (Program p) Operand 
			deriving (Eq, Show)

-- Tipo Grafo é uma lista de Nodes, que em si é uma tupla (Vertice Origem, Vertice Destino, Aresta), onde cada objeto da tupla é uma string
type Graph = [Node]


-- Uma representação melhor do grafo [uso: putStrLn (drawGraph var) ]
drawGraph :: Graph -> String
drawGraph ((a,b,c):gr) = a ++ "--" ++ c ++ "-->" ++ b ++ "\n" ++ drawGraph gr
drawGraph [] = ""

		 -- deriving (Eq,Show)

drawOp :: Operator -> String
drawOp SEQ = ";"
drawOp ND = "U"
drawOp FR = "*"

-- Uma representação melhor do programa [uso: putStrLn (drawProg var) ]
drawProg :: Program p -> String
drawProg (POO op a b)= "(" ++ (drawOp op) ++ "(" ++ a ++ "," ++ b  ++ "))" 
drawProg (POP op a p) = "(" ++ (drawOp op) ++ "(" ++ a ++ "," ++ (drawProg p) ++ "))" 




graph2 = [("1","2", "a"), ("1","3", "b"), ("1","4", "b"), ("1","5", "d"),
				("2","6", "a"), ("2","7", "a"), ("4","8", "d"), ("5","9", "c")]

prog1 = POO SEQ "b" "c" -- (a;b)
prog2 = POP SEQ "a" (POP SEQ "a" (POP ND "b" (POO FR "d" "a"))) -- a;(a;(b U (d;a)*))
seqprog1 = POP SEQ "b" (POP SEQ "b" (POP SEQ "c" (POO SEQ "d" "e"))) -- (b;b;c;d;e)

prog_A = POP SEQ "b" (POP SEQ "b" (POP SEQ "c" (POO SEQ "d" "e"))) -- (b;b;c;d;e)
graph_A = [("1","2","b"),("2","3","b"),("3","4","c"),("4","5","e"),("4","5","d"),("5","6","e")]

prog_B = POP SEQ "b" (POP SEQ "c" (POO SEQ "d" "e")) -- (b;b;c;d;e)
graph_B = [("1","3","b"),("2","3","b"),("3","4","c"),("4","5","e"),("4","5","d"),("5","6","e")]

prog_C = POP SEQ "a" (POP SEQ "b" (POO SEQ "c" "d")) -- (b;b;c;d;e)
graph_C = [("1","2","b"),("1","3","b"),("1","4","a"),("2","5","e"),("4","5","b"),("5","6","a"),
			("5","7","b"),("5","10","c"),("7","8","a"),("10","11","a"),("10","12","d")]

prog_D = POP SEQ "a" (POP SEQ "b" (POP SEQ "c" (POO ND "f" "g"))) -- (a;b;c;(f U g))
graph_D = [("1","2","b"),("1","3","b"),("1","4","a"),("2","5","e"),("4","5","b"),
			("5","10","c"),("10","11","a"),("10","12","f")] --  Problema  quando ultimo no valido e o primeiro e não o segundo (10, 12, f) da erro mas (10 12 g) nao

prog_E = POP SEQ "a" (POP ND "b" (POP SEQ "c" (POO SEQ "f" "g"))) -- (a;b;c;(f U g))
graph_E = [("1","2","b"),("1","3","b"),("1","4","a"),("2","5","e"),("4","5","b"),
			("5","10","c"),("10","11","a"),("10","12","g")]


check :: Program p -> [Char] -> Graph -> [Char]
check (POO _ a _ ) _ [] = "\nFalse:  expected " ++ "x--" ++ a ++ "-->y but the graph is over!\n" 
-- check  _ _ _ = "No can do"

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

-- ;((U(a, b), c) --> ((a U b); c)

check (POO op f g) node ((a,b,c):gr) = --  (POO ND "f" "g"))) 10 ("10","12","f")]  
	if a /= node 
	 	then check (POO op f g) node gr
	else do	
		if op == SEQ  -- if last op is ; 
			then do
				let newEdge = (node, b, f)
				if newEdge == (a, b, c)
					then do
						if g == "END_OF_PROGRAM"
							then "True - Final node is " ++ drawGraph [(a,b,c)]
						else do 
							let end = "END_OF_PROGRAM"
							check (POO op g end) b ((a,b,c):gr)
				else check (POO op f g) node gr
		else do
			if op == ND -- if last op is U
				then do
					if g == "END_OF_PROGRAM"
						then do 
							let newEdge = (node, b, f)
							if newEdge == (a, b, c)
								then "True- possible final node is " ++ drawGraph [(a,b,c)]
							else check (POO op f g) node gr
					else do
						let newEdge = (node, b, f)
						if newEdge == (a, b, c)
							then "True - Second possible final node is " ++ drawGraph [(a,b,c)]
						else do
							check (POO op f g) node (gr)
							check (POO op g "END_OF_PROGRAM") node ((a,b,c):gr)		
						

			else "FR NOT IMPLEMENTED"


check (POP op f p) index ((a,b,c):gr) = -- ( 5, 10 c)  10 ((POP SEQ "c" (POO SEQ "f" "g")))
	if a /= index 
	 	then check (POP op f p) index gr
	else do 
		if op == SEQ
			then do 
				let newEdge = (index, b, f)
				if newEdge == (a, b, c)
					then check p b ((a,b,c):gr) 
				else check (POP op f p) index gr
		else do
			if op == ND 
				then do 
					let newEdge = (index, b, f)	
					if newEdge == (a, b, c)
						then check p b ((a, b, c): gr)
					else do
						check p index ((a,b,c):gr)

			else "Nope"






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


getFR :: Program p -> Program p
getFR (POP op _ _) = (POP op "c" (POO op "a" "b"))  