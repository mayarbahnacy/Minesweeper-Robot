type Cell = (Int,Int,Int)
type Cell1= (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

up:: MyState -> MyState
up (S (x,y,n) l s m)     |x==0 = Null
                         |otherwise= (S (x-1,y,n) l "up" (S (x,y,n) l s m))
down:: MyState -> MyState
down (S (x,y,n) l s m)   |x==n-1 = Null
                         |otherwise= (S (x+1,y,n) l "down" (S (x,y,n) l s m))
left:: MyState -> MyState
left (S (x,y,n) l s m)     |y==0 = Null
                         |otherwise= (S (x,y-1,n) l "left" (S (x,y,n) l s m))
right:: MyState -> MyState
right (S (x,y,n) l s m)  |y==n-1 = Null
                          |otherwise= (S (x,y+1,n) l "right" (S (x,y,n) l s m))	
collect2:: Cell-> [Cell]->[Cell]
collect2 p []= []
collect2 (x,y,n) ((mx,my,mn):xs) | mx==x && my==y = [(mx,my,mn)] 
                           	 |otherwise= collect2 (x,y,n) xs
cleanUp::Cell-> [Cell]-> [Cell]
cleanUp (x,y,n) ((mx,my,mn):xs) | mx==x && my==y = xs 
                           |otherwise = ([(mx,my,mn)]++cleanUp(x,y,n) xs)
collect:: MyState -> MyState
collect (S p l s m) | collect2 p l== [] = Null  
                    | otherwise= (S p (cleanUp p l) "collect"  (S p l s m))

nextMyStates2:: [MyState]->[MyState]
nextMyStates2 []= []
nextMyStates2 (x:xs) | x/=Null = x:nextMyStates2 xs 
                     | otherwise= nextMyStates2 xs
nextMyStates:: MyState->[MyState]
nextMyStates m = nextMyStates2((up m):(down m):(left m):(right m):(collect m): [])

isGoal::MyState->Bool
isGoal (S p l s m) = l== []

search::[MyState]-> MyState 
search (x:xs)  | isGoal x=x
                | otherwise = search(xs++ (nextMyStates x))

constructSolution:: MyState ->[String]
constructSolution (S p l "" m) = []
constructSolution (S p l s m) = (constructSolution m++[s])

solve :: Cell->[Cell]-> [String]
solve p m = constructSolution(search(nextMyStates (S p m "" Null)))

convertCell1toCell:: [Cell1]-> Int-> [Cell]
convertCell1toCell [] n = [] 
convertCell1toCell ((x,y):cs) n = (x,y,n):convertCell1toCell cs n

main :: IO()
main = do 
        putStrLn "Please choose the size of the grid"
        input1 <- getLine
        let n= (read input1 :: Int)
        putStrLn "Enter x-coordinates of Robot"
        input2 <- getLine
        let x = (read input2 :: Int)
	putStrLn "Enter y-coordinates of Robot"
        input3 <- getLine
        let y = (read input3 :: Int)
        putStrLn "enter locations of mines in the form of [(mine 1 x-coordinates,mine 1 y-coordinates), …, (mine n x-coordinates,mine n y-coordinates)]"
        input4 <- getLine
        let l = (read input4 :: [Cell1])
	let p= (x,y,n)
	

        print (solve p (convertCell1toCell l n))
        putStrLn "Thanks"