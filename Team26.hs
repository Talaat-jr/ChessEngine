type Location = (Char,Int)
data Player = White | Black deriving (Show,Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show,Eq)
type Board = (Player,[Piece],[Piece])
-----------
-- Setting the intial state of the chess board
setBoard :: Board 
setBoard = (White ,[],[])
-----------
-- trun pieces lists into board format
visualizeBoard :: Board -> string 
visualizeBoard (player,)


----------
-- check the illegability of moving given piece into the given location
getLocation :: Piece -> Location
getLocation (P loc) = loc
getLocation (N loc) = loc
getLocation (K loc) = loc
getLocation (Q loc) = loc
getLocation (R loc) = loc
getLocation (B loc) = loc

checkValid :: [Piece] -> Location -> Bool
checkValid [] location = True
checkValid (x:xs) location  | getLocation x ==location = False  
                            | otherwise = checkValid xs location

isEmpty :: Location -> Board -> Bool
isEmpty location (player , black , white) = isEmptyHelper location black white

isEmptyHelper :: Location -> [Piece] -> [Piece] -> Bool
isEmptyHelper _ [] [] = True
isEmptyHelper location (h:t) [] | location == (getLocation h) = False   
                                | otherwise = isEmptyHelper location t []
isEmptyHelper location [] (h:t) | location == (getLocation h) = False
                                | otherwise = isEmptyHelper location [] t 
isEmptyHelper location (h1:t1) (h2:t2) | location == (getLocation h1) = False
                                       | location == (getLocation h2) = False
                                       | otherwise = isEmptyHelper location t1 t2

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

checkDiagonal :: Location -> Location -> Board -> Bool
checkDiagonal loc1 loc2 board = (checkDiagonalRightUp 1 loc1 loc2 board) || (checkDiagonalRightDown 1 loc1 loc2 board) || (checkDiagonalLeftUp 1 loc1 loc2 board) || (checkDiagonalLeftDown 1 loc1 loc2 board)

checkDiagonalRightUp :: Int -> Location -> Location -> Board -> Bool
checkDiagonalRightUp i (c1 , r1) (c2,r2) board  | (isEmpty ((setIndex ((getIndex c1) + i)),(r1+i)) board) && (setIndex ((getIndex c1) + i)) ==c2 && (r1+i) == r2 = True
                                                | (getIndex (c1) +i) > 7 || (r1+i) > 8 = False
                                                | (isEmpty ((setIndex ((getIndex c1) + i)),(r1+i)) board) && not ((setIndex ((getIndex c1) + i)) ==c2 && (r1+i) == r2)  = checkDiagonalRightUp (i+1) (c1 , r1) (c2,r2) board
                                                | not (isEmpty ((setIndex ((getIndex c1) + i)),(r1+i)) board) &&  ( ((getIndex c1) + i)) ==(getIndex c2) && (r1+i) == r2 = True
                                                | otherwise = False

checkDiagonalRightDown :: Int -> Location -> Location -> Board -> Bool
checkDiagonalRightDown i (c1 , r1) (c2,r2) board  | (isEmpty ((setIndex ((getIndex c1) + i)),(r1-i))  board) && (setIndex ((getIndex c1) + i)) ==c2 && (r1-i) == r2 = True
                                                  | (getIndex (c1) +i) > 7 || (r1-i) < 1 = False
                                                  | (isEmpty ((setIndex ((getIndex c1) + i)),(r1-i))  board) && not ((setIndex ((getIndex c1) + i)) ==c2 && (r1-i) == r2)= checkDiagonalRightDown (i+1) (c1 , r1) (c2,r2) board
                                                  | not (isEmpty ((setIndex ((getIndex c1) + i)),(r1+i)) board) && ( ((getIndex c1) + i)) ==(getIndex c2) && (r1-i)==r2 = True
                                                  | otherwise = False

checkDiagonalLeftDown :: Int -> Location -> Location -> Board -> Bool
checkDiagonalLeftDown i (c1 , r1) (c2,r2) board   | (isEmpty ((setIndex ((getIndex c1) - i)),(r1-i))  board) && (setIndex ((getIndex c1) - i)) ==c2 && (r1-i)== r2 = True
                                                  | (getIndex (c1) -i) < 0 || (r1-i) < 1 = False
                                                  | (isEmpty ((setIndex ((getIndex c1) - i)),(r1-i))  board) && not((setIndex ((getIndex c1) - i)) ==c2 && (r1-i)== r2) = checkDiagonalLeftDown (i+1) (c1 , r1) (c2,r2) board
                                                  | not (isEmpty ((setIndex ((getIndex c1) - i)),(r1-i))  board) && ( ((getIndex c1) - i)) ==(getIndex c2) && (r1-i)== r2 = True
                                                  | otherwise = False

checkDiagonalLeftUp :: Int -> Location -> Location -> Board -> Bool
checkDiagonalLeftUp i (c1 , r1) (c2,r2) board     | (isEmpty ((setIndex ((getIndex c1) - i)),(r1+i))  board) && (setIndex ((getIndex c1) - i)) ==c2 && (r1+i)== r2 = True
                                                  | (getIndex (c1) -i) < 0 || (r1+i) > 8 = False
                                                  | (isEmpty ((setIndex ((getIndex c1) - i)),(r1+i))  board) && not ((setIndex ((getIndex c1) - i)) ==c2 && (r1+i)== r2) = checkDiagonalLeftUp (i+1) (c1 , r1) (c2,r2) board
                                                  | not (isEmpty ((setIndex ((getIndex c1) - i)),(r1+i))  board) && ( ((getIndex c1) - i)) ==(getIndex c2) && (r1+i)== r2 = True
                                                  | otherwise = False

isLegal :: Piece -> Board -> Location -> Bool

isLegal (R currLoc) board newLoc = rookLegalHelper  (R currLoc) board newLoc
isLegal (B currLoc) board newLoc = bishopLegalHelper (B currLoc) board newLoc
isLegal (Q currLoc) board newLoc = rookLegalHelper (R currLoc) board newLoc || bishopLegalHelper (B currLoc) board newLoc




rookLegalHelper :: Piece -> Board -> Location -> Bool
rookLegalHelper (R (c1,r1)) (player , white ,black) (c2,r2)     | c1/= c2 && r1/=r2 = False
                                                                | r1==r2 && c1 > c2 && player==Black = checkValid black (c2,r2) && rookLocationColumn_Left (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | r1==r2 && c1 > c2 && player==White = checkValid white (c2,r2) && rookLocationColumn_Left (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | r1==r2 && c1 < c2 && player==Black = checkValid black (c2,r2) && rookLocationColumn_Right (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | r1==r2 && c1 < c2 && player==White = checkValid white (c2,r2) && rookLocationColumn_Right (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | c1==c2 && r1 > r2 && player==Black = checkValid black (c2,r2) && rookLocationRow_Down (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | c1==c2 && r1 > r2 && player==White = checkValid white (c2,r2) && rookLocationRow_Down (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | c1==c2 && r1 < r2 && player==Black = checkValid black (c2,r2) && rookLocationRow_Up (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | c1==c2 && r1 < r2 && player==White = checkValid white (c2,r2) && rookLocationRow_Up (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                
rookLocationRow_Up :: Piece -> Board -> Location -> Bool
rookLocationRow_Up (R (c1,r1)) board (c2,r2)| r2==r1 = True
                                            | isEmpty (c1 , (r1 + 1)) board = rookLocationRow_Up (R (c1,r1+1)) board (c2,r2) 
                                            | not (isEmpty (c1 , (r1 + 1)) board) && (r1+1)==r2 = True
                                            | otherwise = False

rookLocationRow_Down :: Piece -> Board -> Location -> Bool
rookLocationRow_Down (R (c1,r1)) board (c2,r2)| r2==r1 = True
                                              | isEmpty (c1 , (r1 - 1)) board = rookLocationRow_Down (R (c1,r1-1)) board (c2,r2) 
                                              | not (isEmpty (c1 , (r1 - 1)) board) && (r1-1)==r2 = True
                                              | otherwise = False  

rookLocationColumn_Left :: Piece -> Board -> Location -> Bool
rookLocationColumn_Left  (R (c1,r1)) board (c2,r2)  | c1 == c2 = True
                                                    | isEmpty ((setIndex((getIndex c1) -1)),r1) board = rookLocationColumn_Left (R ((setIndex((getIndex c1) -1)),r1)) board (c2,r2) 
                                                    | not (isEmpty ((setIndex((getIndex c1) -1)),r1) board) && ((getIndex c1) -1) ==(getIndex c2) = True
                                                    | otherwise = False

rookLocationColumn_Right :: Piece -> Board -> Location -> Bool
rookLocationColumn_Right  (R (c1,r1)) board (c2,r2) | c1 == c2 = True
                                                    | isEmpty ((setIndex((getIndex c1) +1)),r1) board = rookLocationColumn_Right (R ((setIndex((getIndex c1) +1)),r1)) board (c2,r2) 
                                                    | not (isEmpty ((setIndex((getIndex c1) +1)),r1) board) &&  ((getIndex c1) + 1) ==(getIndex c2) = True
                                                    | otherwise = False

bishopLegalHelper :: Piece -> Board -> Location  -> Bool
bishopLegalHelper (B (c1,r1)) (player , white ,black) (c2,r2)   | player==White = checkValid white (c2,r2)  && checkDiagonal (c1,r1) (c2,r2) (player , white ,black)
                                                                | player==Black = checkValid black (c2,r2)  && checkDiagonal (c1,r1) (c2,r2) (player , white ,black)




----------
-- retuen list of illegal valid move for given piece
suggestMove :: Piece -> Board -> [Location]




----------
-- return the board after moving the given piece 
move :: Piece -> Location -> Board -> Board 