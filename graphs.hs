type Vale = [Char]
type Vertice = [Char]
type Aresta = (Vertice, Vertice, Vale)

type Grafo = [Aresta]

grafoTeste :: [Aresta]
grafoTeste = [
			("1","2", "a"), ("1","3", "b"), ("1","4", "b"), ("1","5", "d"),
			("2","6", "a"), ("2","7", "a"), ("4","8", "d"), ("5","9", "c")
		]

adjacentes :: Grafo -> Vertice -> [Vertice] 
adjacentes [] _ = []
adjacentes ((a,b,p):c) v =
	if a == v
		then do
			b:(adjacentes c v)
	else do
		if b == v
			then do
				a:(adjacentes c v)
		  	else do 
		  		adjacentes c v


valido :: Grafo -> Aresta -> [(Aresta,Vertice)]
valido [] _ = []
valido ((a,b,c):r) (e,f,g)
	| ((c == g) && (a == e)) = ((a,b,c), b):valido r (e,f,g)
	| otherwise = valido r (e,f,g)
	