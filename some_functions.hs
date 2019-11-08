import Data.List
import System.IO


-- primeNumbers = [3,5,7,11]

-- morePrime = primeNumbers ++ [13, 17, 19, 23, 29]

-- favNums = 2 : 7 : 21 : 66 :[]

-- multiList = [[3,5,7],[11,13,17]]

-- morePrimes2 = 2 : morePrime


-- divisby2n3n5 = [x | x <- [1..300], x `mod` 2 == 0, x `mod` 3 == 0, x `mod` 5 == 0]

-- sortedList = sort[9,1,8,4,10,3] 

-- sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]

-- listof90 = [x | x <- divisby2n3n5, x `mod` 9 == 0]

-- listLowerThan200 = filter(<200) divisby2n3n5

-- evenUptTo20 = takeWhile (<= 20) [2,4..]

-- sumOfEvensUpto200 = foldl (+) 1 (takeWhile (<=200) [2,4..])


-- pow3List = [3^n | n <- [1..10]]

-- sumOfEvensUpto200;

-- funcName param1 param2 = operations ( returned values)
addMe :: Int -> Int -> Int
addMe x y = x + y
 

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci :: Int -> Int
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n =  fibonacci(n -1) + fibonacci(n - 2)

batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
	| avg <= 0.200 = "meh"
	| avg <= 0.250 = "ok"
	| avg <= 0.280 = "good"
	| otherwise = "great"
	where avg = hits / atBats

-- case estatments
get5or6 :: Int -> String 
get5or6 n = case n of
	5 -> "Five"
	6 -> "Six"
	_ -> "Others"


areStringEq :: [Char] -> [Char] -> Bool
areStringEq [] [] = True
areStringEq (x:xs) (y:ys) = x == y && areStringEq xs ys
areStringEq _ _ = False

doMult :: Int -> (Int -> Int) -> Int
doMult n func = func n

double1to10 = map (\x -> x * 2) [1..10]


data BaseballPlayer = Pitcher | Catcher | Infielder | Outfield deriving Show


readFromFile = do
	theFile <- openFile "test.txt" ReadMode
	contents <- hGetContents theFile
	putStr contents
	hClose theFile

fatorial :: Int -> Int
fatorial 1 = 1
fatorial x = x * fatorial (x-1)

andOp :: Bool -> Bool -> Bool
andOp False _ = False
andOp _ False = False
andOp True True = True

grafo = [("n0","n1","x"), ("n1","n2","x"), ("n1","n2","y")]


forLoop n maxN value =
	if n < 10
		then forLoop (n +1) maxN (n + 1)
		else value 

forEach :: String -> Int
forEach [] = 0
forEach (char:string) = 1 + (forEach string)

whileLoop :: Int -> Int
whileLoop a =
	if a > 0
		then whileLoop(a - 1)
		else a

-- first10div5 = tuple( head take 10 [x | x <- [1..100], x `mod` 5 == 0]
-- manySimples = replicate simpleList 3
