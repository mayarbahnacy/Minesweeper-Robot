type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)
up:: MyState -> MyState
up (S (x,y) l s m)     |x==0 = Null
                       |otherwise= (S (x-1,y) l "up" (S (x,y) l s m))
down:: MyState -> MyState
down (S (x,y) l s m)     |x==3 = Null
                         |otherwise= (S (x+1,y) l "down" (S (x,y) l s m))
left:: MyState -> MyState
left (S (x,y) l s m)     |y==0 = Null
                         |otherwise= (S (x,y-1) l "left" (S (x,y) l s m))
right:: MyState -> MyState
right (S (x,y) l s m)     |y==3 = Null
                          |otherwise= (S (x,y+1) l "right" (S (x,y) l s m))	
collect2:: Cell-> [Cell]->[Cell]
collect2 p []= []
collect2 (x,y) ((mx,my):xs) | mx==x && my==y = [(mx,my)] 
                            |otherwise= collect2 (x,y) xs
cleanUp::Cell-> [Cell]-> [Cell]
cleanUp (x,y) ((mx,my):xs) | mx==x && my==y = xs 
                           |otherwise = ([(mx,my)]++cleanUp(x,y) xs)
collect:: MyState -> MyState
collect (S p l s m) | collect2 p l== [] = Null  
                    | otherwise= (S p (cleanUp p l) "collect" (S p l s m))

nextMyStates2:: [MyState]->[MyState]
nextMyStates2 []= []
nextMyStates2 (x:xs) | x/=Null = x:nextMyStates2 xs 
                     | otherwise= nextMyStates2 xs
nextMyStates::MyState->[MyState]
nextMyStates m= nextMyStates2((up m):(down m):(left m):(right m):(collect m): [])
isGoal::MyState->Bool
isGoal (S p l s m) = l== []
search::[MyState]->MyState
search (x:xs) | isGoal x=x
              | otherwise = search(xs++ nextMyStates x)
constructSolution:: MyState ->[String]
constructSolution (S p l "" m) = []
constructSolution (S p l s m) = (constructSolution m++[s])
solve :: Cell->[Cell]->[String]
solve p m= constructSolution(search(nextMyStates(S p m "" Null)))