module SodukuSolver where
import Problems
import Solver
import Data.List
import Data.Bits

--1 // Data type
data SudokuConfig = SudokuConfig [Integer]

--2 // SudokuConfigFromList function
--This function takes list of integers and returns sudoku configuration, zero represents blank
sudokuConfigFromList::[Integer]->SudokuConfig
sudokuConfigFromList [] = SudokuConfig []
sudokuConfigFromList (x:xs) = SudokuConfig (x: (listFromSudokuConfig (sudokuConfigFromList xs)))
--eg 1.  sudokuConfigFromList [1,2,3,4,0,2,3,4,1,2,3,4,5,6,6,7,9,0,1,2,3,4,5,6,7,8,9,0,1,2]


 --1 2 3   4 _ 2   3 4 1  

 --2 3 4   5 6 6   7 9 _  

 --1 2 3   4 5 6   7 8 9  


 --_ 1 2  

-- 3 // This function takes sudoku configuration and return list of integers
listFromSudokuConfig::SudokuConfig->[Integer]
listFromSudokuConfig (SudokuConfig list) = list
--eg 1 listFromSudokuConfig (SudokuConfig [1,2,3])
--[1,2,3]

--eg 2. listFromSudokuConfig (SudokuConfig [1,2,3,4,5])
--	[1,2,3,4,5]

--4 // Instance of Eq
instance Eq SudokuConfig where
	 SudokuConfig x == SudokuConfig y =	 (x == y)

--5 //Instance of Show
instance Show SudokuConfig where
	show(SudokuConfig config) = prettyPrint config 0

prettyPrint::[Integer]->Integer->[Char]
prettyPrint [] counter= [] 
prettyPrint config 3="\n"++(prettyPrint config 0) 
prettyPrint config counter="\n"++(printl(take 9 config) "\n" 0)++prettyPrint(drop 9 config)(counter + 1)

--eg 1 prettyPrint [1,2,3,4,5] 3
--	"\n\n\n 1 2 3   4 5  "
--eg 2  prettyPrint [1,2,3,4,5,6,7,8,9] 3
--	"\n\n\n 1 2 3   4 5 6   7 8 9  "


printl::[Integer]->[Char]->Integer->[Char]
printl [] string _  = string++ "  "
printl x  string 3 = printl x (string++ "  ") 0
printl (x:xs) string counter = (printl xs (string ++ " "++ (intToChar x)) (counter + 1)) 
--eg 1 printl [1,2,3,4,5] ['1','2','3','4'] 3
--	"1234   1 2 3   4 5  "

intToChar::Integer->[Char]
intToChar x
	|x==0 = "_"
	|otherwise = show x
--eg 1 intToChar 0
-- "_"

--eg 2 intToChar 2
--"2"

--6 Successor function------------------------------------
instance Config SudokuConfig where
	successors (SudokuConfig []) = []
	successors (SudokuConfig lst) = [SudokuConfig (putNumber lst (findZero lst 0) x)|
				x<-removeelem((getElementByList lst (cellLookup (findZero lst 0))))]

{-
successors (SudokuConfig trivial)
[

 1 4 6   _ _ _   8 9 _  

 _ 7 _   4 _ 9   _ 1 _  

 5 _ _   _ 8 _   _ _ 6  


 _ _ 3   9 _ 8   6 _ _  

 9 _ _   _ _ _   _ _ 2  

 _ _ 8   5 _ 2   1 _ _  


 4 _ _   _ 5 _   _ _ 3  

 _ 2 _   1 _ 6   _ 7 _  

 _ 9 7   _ _ _   5 2 _  ,

 2 4 6   _ _ _   8 9 _  

 _ 7 _   4 _ 9   _ 1 _  

 5 _ _   _ 8 _   _ _ 6  


 _ _ 3   9 _ 8   6 _ _  

 9 _ _   _ _ _   _ _ 2  

 _ _ 8   5 _ 2   1 _ _  


 4 _ _   _ 5 _   _ _ 3  

 _ 2 _   1 _ 6   _ 7 _  

 _ 9 7   _ _ _   5 2 _  ,

 3 4 6   _ _ _   8 9 _  

 _ 7 _   4 _ 9   _ 1 _  

 5 _ _   _ 8 _   _ _ 6  


 _ _ 3   9 _ 8   6 _ _  

 9 _ _   _ _ _   _ _ 2  

 _ _ 8   5 _ 2   1 _ _  


 4 _ _   _ 5 _   _ _ 3  

 _ 2 _   1 _ 6   _ 7 _  

 _ 9 7   _ _ _   5 2 _  ]
-}

--Retrieve elements other than in the list
removeelem::[Integer]->[Integer]
removeelem lst=[x|x<-[1..9] , not(x `elem` lst)]
-- eg 1 removeelem [1,2,3]
-- [4,5,6,7,8,9]

--eg 2   removeelem [1,2,3,8,9,4]
--	[5,6,7]
 
--Put Number in place of Zero in the list
putNumber::[Integer]->Integer->Integer->[Integer]
putNumber list pos num = leftlist++[num]++rightList
			where leftlist = take (fromIntegral pos) list
			      rightList = drop ((fromIntegral pos)+1) list
--eg1 putNumber [1,2,3,4] 2 5
-- 	[1,2,5,4]

-- eg2 putNumber [1,2,3] 1 3
--	[1,3,3]

--Find zero in the list
findZero::[Integer]->Integer->Integer
findZero [] _ = -1
findZero (x:xs) num 
		| x==0 = num
		| otherwise = findZero xs (num+1)
--eg 1 findZero [1,2,3,4,5,0] 0
--	5

--eg 2  findZero [1,2,0,3,4] 0
--	2

--Gets element at gven position in the list
getElement::[Integer]->Integer->Integer->Integer
getElement (x:xs) start position
		|start==position = x
		|otherwise = getElement xs (start+1) position

--eg 1  getElement [1,2,3,4,5] 1 3
--	3

-- eg 2 [10,11,2,3,4] 1 3
--	2


getElementByList::[Integer]->[Integer]->[Integer]
getElementByList list [] = []
getElementByList list (x:xs) = (getElement list 0 x):(getElementByList list xs)

--eg 1  getElementByList [1,2,3,4,5][1,2,3,4]
-- 	[2,3,4,5]

-- eg 2  getElementByList [1,2,3][1,2]
--	[2,3]

-------------------------------------
-- 7 

--Is Sudoku Goal?
isSudokuGoal::SudokuConfig->Bool
isSudokuGoal (SudokuConfig []) = False
isSudokuGoal (SudokuConfig intList) =  isComplete intList && satisfy intList intList 0

-- eg1 isSudokuGoal (SudokuConfig trivial)
--	False

--eg 2  isSudokuGoal (SudokuConfig trivial)
--False


isComplete::[Integer]->Bool
isComplete list = count' 0 list 0 ==0
--eg 1 isComplete trivial
--False
--eg 2  isComplete solvd
--True

satisfy :: [Integer]->[Integer]->Integer->Bool
satisfy xs ls 80 = check 80 ls
satisfy xs ls num = check num ls && (satisfy xs ls (num+1))
{-
satisfy trivial trivial 0
False

 satisfy solvd solvd 0
True

-}

check:: Integer->[Integer] ->Bool
check x list = count'( getElement list 0 x)  (getElementByList list (cellLookup x)) 0 == 1

{-check 1 trivial
True
-}

--eg 1 

count'::Integer->[Integer]->Integer->Integer
count' x [] num = num
count' x (y:ys) num
		| x==y =count' x ys (num+1)
		|otherwise = count' x ys num

--eg 1 count' 2 [1,2,3,4] 2
--	3


getColumn::Integer->[Integer]
getColumn number = helpColumn number 0

--eg 1  getColumn 2
-- 	[2,11,20,29,38,47,56,65,74]

helpColumn::Integer->Integer->[Integer]
helpColumn _ 9 = []
helpColumn number counter = number: helpColumn (number+9)(counter+1)
{-
 helpColumn 1 0
[1,10,19,28,37,46,55,64,73]
-}
getRow::Integer->[Integer]
getRow number = helpRow (9*number) 0
-- eg 1.	getRow 3
--[27,28,29,30,31,32,33,34,35]


helpRow::Integer->Integer->[Integer]
helpRow _ 9 =[]
helpRow number counter = number+counter:helpRow number (counter+1)
-- eg 1. helpRow 1 1
-- [2,3,4,5,6,7,8,9]


getBlockNums :: Integer->[Integer]
getBlockNums n = traverse (blockStartPos n) 0 0 0
{- 
getBlockNums 2
[6,7,8,15,16,17,24,25,26]
-} 

blockStartPos::Integer->Integer
blockStartPos num 
	| num <= 2 = num*3
	| num <= 5  = 27 + (num * 3 - 9)
	| num <= 8    = 54 + (num * 3 - 18)
{-
blockStartPos 1
3
-}	

traverse::Integer->Integer->Integer->Integer->[Integer]
traverse _ _ _ 9 = []
traverse pos num1 num2 counter = number : traverse pos cell column (counter + 1)
				where number = pos + num2 + num1
                                      cell= if num2 == 2 then num1 + 9 else num1
              	                      column= if num2 == 2 then 0 else num2 + 1
{-
 traverse 1 3 4 2
[8,9,10,11,12,13,14]
-}

blockLookup ::Integer->[Integer]
blockLookup x = getNumBlocks x 0
{-
 eg 1 - blockLookup 1
	[0,1,2,9,10,11,18,19,20]
-}
              

getNumBlocks::Integer->Integer->[Integer]
getNumBlocks _ 9 = error "Invalid Block"
getNumBlocks num counter = if num `elem` (getBlockNums counter) then (getBlockNums counter) else getNumBlocks num (counter+1)

{-
getNumBlocks 1 0
[0,1,2,9,10,11,18,19,20]
-}

cellLookup::Integer->[Integer]
cellLookup num = nub ((rowLookup num) ++ (columnLookup num) ++ (blockLookup num))
{-

for eg. cellLookup 2
	[0,1,2,3,4,5,6,7,8,11,20,29,38,47,56,65,74,9,10,18,19]
-}

rowLookup :: Integer -> [Integer]
rowLookup cell = locateCellRow cell 0      
{-
eg 1. rowLookup 1
	[0,1,2,3,4,5,6,7,8]
-}
locateCellRow :: Integer ->Integer-> [Integer]
locateCellRow _ 9 = error "Invalid Row" 
locateCellRow cell counter = if cell `elem` getRow counter then getRow counter else locateCellRow cell (counter+1)

{-
eg 1.	locateCellRow 1 0
	[0,1,2,3,4,5,6,7,8]
-}
columnLookup :: Integer -> [Integer]
columnLookup cell = locateCellColumn cell 0
{-
eg 1.   ColumnLookup 1
	[1,10,19,28,37,46,55,64,73]
-}

locateCellColumn::Integer->Integer->[Integer]
locateCellColumn _ 9 = error "Invalid Column"
locateCellColumn cell counter = if cell `elem` getColumn counter then getColumn counter else locateCellColumn cell 													(counter+1)
 {-	 locateCellColumn 1 0
	[1,10,19,28,37,46,55,64,73]

-}         

-- 8 SudokSolver
sudokuSolve:: SudokuConfig->(Maybe SudokuConfig)
sudokuSolve config = 
	let isGoal = (isSudokuGoal)
	in  solve isGoal config


--eg 1 sudokuSolve  (SudokuConfig trivial)
-- Just 

-- 1 4 6   3 2 5   8 9 7  

-- 8 7 2   4 6 9   3 1 5  

-- 5 3 9   7 8 1   2 4 6  


-- 2 1 3   9 7 8   6 5 4  

-- 9 5 4   6 1 3   7 8 2  

-- 7 6 8   5 4 2   1 3 9  


-- 4 8 1   2 5 7   9 6 3  

-- 3 2 5   1 9 6   4 7 8  

-- 6 9 7   8 3 4   5 2 1 

-- eg 2 sudokuSolve (SudokuConfig diabolical)
-- Just 

-- 8 5 3   7 9 1   6 4 2  

-- 9 2 6   5 4 8   7 3 1  

-- 4 1 7   3 6 2   8 9 5  


-- 5 6 4   1 7 3   2 8 9  

-- 7 3 9   8 2 4   1 5 6  

-- 1 8 2   9 5 6   3 7 4  


-- 6 9 5   2 3 7   4 1 8  

-- 2 7 8   4 1 9   5 6 3  

-- 3 4 1   6 8 5   9 2 7  





