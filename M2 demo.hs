type Location = (Char,Int)
data Player = White | Black deriving (Show,Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show,Eq)
type Board = (Player,[Piece],[Piece])

allLocations = [('a',1) , ('a',2) , ('a',3) , ('a',4) , ('a',5) , ('a',6) , ('a',7) , ('a',8),
                ('b',1) , ('b',2) , ('b',3) , ('b',4) , ('b',5) , ('b',6) , ('b',7) , ('b',8),
                ('c',1) , ('c',2) , ('c',3) , ('c',4) , ('c',5) , ('c',6) , ('c',7) , ('c',8),
                ('d',1) , ('d',2) , ('d',3) , ('d',4) , ('d',5) , ('d',6) , ('d',7) , ('d',8),
                ('e',1) , ('e',2) , ('e',3) , ('e',4) , ('e',5) , ('e',6) , ('e',7) , ('e',8),
                ('f',1) , ('f',2) , ('f',3) , ('f',4) , ('f',5) , ('f',6) , ('f',7) , ('f',8),
                ('g',1) , ('g',2) , ('g',3) , ('g',4) , ('g',5) , ('g',6) , ('g',7) , ('g',8),
                ('h',1) , ('h',2) , ('h',3) , ('h',4) , ('h',5) , ('h',6) , ('h',7) , ('h',8),]
-----------
-- Setting the intial state of the chess board
setBoard :: Board 
setBoard = (White ,[R ('h',1),N ('g',1),B ('f',1),K ('e',1),
Q ('d',1),B ('c',1),N ('b',1),R ('a',1),
P ('h',2),P ('g',2),P ('f',2),P ('e',2),
P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,
[R ('h',8),N ('g',8),B ('f',8),K ('e',8),
Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
P ('h',7),P ('g',7),P ('f',7),P ('e',7),
P ('d',7),P ('c',7),P ('b',7),P ('a',7)])
-----------
-- trun pieces lists into board format
visualizeBoard :: Board -> string 
visualizeBoard (player,white,black) = 



["|  |","|  |","|  |","|  |","|  |","|  |","|  |","|  |"]       = "| "++getType(p)++getColor(P)++" |"

-- toString :: Char -> [Piece] -> String
-- toString 'i' list = ""
-- toString char list = (search )

-- search char ((P (char,n)):t) = 

toString :: 
toString list acc =  



overrideElement :: [a] -> a -> Int -> [a]
overrideElement list x i = let (ys, _:zs) = splitAt i list in ys ++ x:zs     

getRow :: Int -> [Piece] -> [Piece]
getRow 0 _ = []
getRow n ((P (char,n)):t) = ((P (char,n))):getRow n t  
getRow n ((N (char,n)):t) = ((N (char,n))):getRow n t 
getRow n ((K (char,n)):t) = ((K (char,n))):getRow n t 
getRow n ((Q (char,n)):t) = ((Q (char,n))):getRow n t 
getRow n ((R (char,n)):t) = ((R (char,n))):getRow n t 
getRow n ((B (char,n)):t) = ((B (char,n))):getRow n t 

getColor ::Piece-> [Piece] -> [Piece] -> String
getColor piece white black |elem piece white = "W"
                           |elem piece black = "B"
                           |otherwise = error "not found"

getType :: Piece -> String
getType (P location) = "P"
getType (N location) = "N"
getType (K location) = "K"
getType (Q location) = "Q"
getType (R location) = "R"
getType (B location) = "B"

getIndex :: Char -> Int
getIndex 'a'=0
getIndex 'b'=1
getIndex 'c'=2
getIndex 'd'=3
getIndex 'e'=4
getIndex 'f'=5
getIndex 'g'=6
getIndex 'h'=7

setIndex :: Int -> Char
setIndex 0='a'
setIndex 1='b'
setIndex 2='c'
setIndex 3='d'
setIndex 4='e'
setIndex 5='f'
setIndex 6='g'
setIndex 7='h'
setIndex n | n>7 = error "index out of range"
           | n<0 = error "index out of range"

----------
-- check the illegability of moving given piece into the given location
isLegal :: Piece -> Board -> Location -> Bool
isLegal (P location) (player ,_,_) locationNew | player==Black = bPawnLegalHelper (P location) board locationNew
                                               | otherwise = wPawnLegalHelper  (P location) board locationNew
isLegal (N location) board locationNew = knightLegalHelper (N location) board locationNew
isLegal (K location) board locationNew = kingLegalHelper (K location) board locationNew
isLegal (Q location) board locationNew = queenLegalHelper (Q location) board locationNew
isLegal (R location) board locationNew = rookLegalHelper (R location)
isLegal (B location) board locationNew = bishopLegalHelper (B location)

bPawnLegalHelper (a,n)  board locationNew   | (n==7) && checkInLocation (a,5) && locationNew == (a,5) = True
                                            | (n==7) && checkInLocation (a,5) && checkInLocation (a,6) && locationNew == (a,6) = True 
                                            | (a != 'a') && (a != 'h') && locationNew == ((predecesor a) (n-1)) && checkInLocationOpposite board ((predecesor a) (n-1)) (a,n) == True
                                            | locationNew == ((succ a) (n-1)) && checkInLocationOpposite board ((succ a) (n-1)) (a,n) == True


knightLegalHelper (K (a,n)) board locationNew    | (a!='g') && (a!='h') && (n!=8) && (isEmpty ((setIndex ((getIndex a) + 2)),n+1)  || checkInLocationOpposite ((setIndex ((getIndex a) + 2)),n+1)) = True
                                                 | (a!='g') && (a!='h') && (n!=1) && (isEmpty ((setIndex ((getIndex a) + 2)),n-1)  || checkInLocationOpposite ((setIndex ((getIndex a) + 2)),n-1)) = True
                                                 | (a!='a') && (a!='b') && (n!=8) && (isEmpty ((setIndex ((getIndex a) - 2)),n+1)  || checkInLocationOpposite ((setIndex ((getIndex a) - 2)),n+1)) = True
                                                 | (a!='b') && (a!='a') && (n!=1) && (isEmpty ((setIndex ((getIndex a) - 2)),n-1)  || checkInLocationOpposite ((setIndex ((getIndex a) - 2)),n-1)) = True
                                                 | (a!='h') && (n!= 8) && (n!=7) && (isEmpty ((setIndex ((getIndex a) + 1)),n+2)  || checkInLocationOpposite ((setIndex ((getIndex a) + 1)),n+2)) = True
                                                 | (a!='h') && (n!= 1) && (n!=2) && (isEmpty ((setIndex ((getIndex a) + 1)),n-2)  || checkInLocationOpposite ((setIndex ((getIndex a) + 1)),n-2)) = True
                                                 | (a!='a') && (n!= 8) && (n!=7) && (isEmpty ((setIndex ((getIndex a) - 1)),n+2)  || checkInLocationOpposite ((setIndex ((getIndex a) - 1)),n+2)) = True
                                                 | (a!='a') && (n!= 1) && (n!=2) && (isEmpty ((setIndex ((getIndex a) - 1)),n-2)  || checkInLocationOpposite ((setIndex ((getIndex a) - 1)),n-2)) = True
                                                 | otherwise = False
                                                 

isEmpty :: Location -> Board -> Bool
isEmpty location (player , black , white) = isEmptyHelper location black white

isEmptyHelper :: Location -> [Piece] -> [Piece] -> Bool
isEmptyHelper _ [] [] = True
isEmptyHelper location [h:t] [] | location == (getLocation h) = False   
                                | otherwise = isEmptyHelper location t []
isEmptyHelper location [] [h:t] | location == (getLocation h) = False
                                | otherwise = isEmptyHelper location [] t 
isEmptyHelper location [h1:t1] [h2:t2] | location == (getLocation h1) = False
                                       | location == (getLocation h2) = False
                                       | otherwise = isEmptyHelper location t1 t2

checkInLocationOpposite :: Location -> Board -> Bool
checkInLocationOpposite location (player , black , white) = checkInLocationOpposite location black white

checkInLocation :: Location -> [Piece] -> [Piece] -> Bool
checkInLocation _ [] [] = False
checkInLocation location [h:t] [] | location == (getLocation h) = True   
                                  | otherwise = checkInLocation location t []
checkInLocation location [] [h:t] | location == (getLocation h) = True
                                    | otherwise = checkInLocation location [] t
checkInLocation location [h1:t1] [h2:t2] | location == (getLocation h1) = True
                                         | location == (getLocation h2) = True
                                         | otherwise = checkInLocation location t1 t2

                                         



checkInLocation :: Location -> Board -> Bool
checkInLocation location (player , black , white) = checkInLocation (P location) black white



isLocated :: Location -> [Piece] -> Bool
isLocated _ [] = True
isLocated location [h:t]    | location == (getLocation h) = False   
                            | otherwise = isLocated location t

getLocation :: Piece -> Location
getLocation (P loc) = loc
getLocation (N loc) = loc
getLocation (K loc) = loc
getLocation (Q loc) = loc
getLocation (R loc) = loc
getLocation (B loc) = loc

----------
-- retuen list of legal valid move for given piece
suggestMove :: Piece -> Board -> [Location]
suggestMove piece board = 

suggestH 'i' piece board = []
suggestH char piece board = (suggestL 1 char piece board) ++ (suggestH (succ char) piece board) 

suggestL 9 char piece board = []
suggestL n char piece board | isLegal piece board (char,n) = (char,n):suggestL (n+1) char piece board
                            | otherwise = suggestL (n+1) char piece board


suggestMove2 :: Piece -> Board -> [Location] -> [Location]
suggestMove2 _ _ [] = []
suggestMove2 piece board (h:t)  | isLegal Piece Board h = h:(suggestMove2 piece board t)
                                | otherwise = (suggestMove2 piece board t)

----------
-- return the board after moving the given piece 
move :: Piece -> Location -> Board -> Board 
move piece location (player,white,black)| not (isLegal piece board location) = error "illegal move for piece "
                                        | not (checkTurn piece board) = error "It's"
                                        | player == White = (Black,updateList white piece location,black)
                                        | player == Black = (White,white,updateList black piece location)

updateList :: [Piece] -> Piece -> Location -> [Piece]
updateList list piece location = (updatePiece piece location):(filter (/= piece) list)

checkTurn :: Piece -> Board -> Bool
checkTurn Piece (player,white,black)    | player==White && elem piece black = False
                                        | player==Black && elem piece white = False
                                        | otherwise = True

updatePiece :: Piece -> Location -> Piece
updatePiece (P l) location = P location
updatePiece (N l) location = N location
updatePiece (K l) location = K location
updatePiece (Q l) location = Q location
updatePiece (R l) location = R location
updatePiece (B l) location = B location
----------
-- check if the given location is in the given list
checkInLocation :: Location -> [Piece] -> Bool
checkInLocation _ [] = False
checkInLocation location ((P loc):t) | location == loc = True
                                     | otherwise = checkInLocation location t
checkInLocation location ((N loc):t) | location == loc = True
                                        | otherwise = checkInLocation location t            
checkInLocation location ((K loc):t) | location == loc = True
                                        | otherwise = checkInLocation location t            
checkInLocation location ((Q loc):t) | location == loc = True
                                        | otherwise = checkInLocation location t
checkInLocation location ((R loc):t) | location == loc = True   
                                        | otherwise = checkInLocation location t    
checkInLocation location ((B loc):t) | location == loc = True
                                        | otherwise = checkInLocation location t    
----------
