type Location = (Char,Int)
data Player = White | Black deriving (Show,Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show,Eq)
type Board = (Player,[Piece],[Piece])

allLocations :: [Location]
allLocations = [('a',1) , ('a',2) , ('a',3) , ('a',4) , ('a',5) , ('a',6) , ('a',7) , ('a',8),
                ('b',1) , ('b',2) , ('b',3) , ('b',4) , ('b',5) , ('b',6) , ('b',7) , ('b',8),
                ('c',1) , ('c',2) , ('c',3) , ('c',4) , ('c',5) , ('c',6) , ('c',7) , ('c',8),
                ('d',1) , ('d',2) , ('d',3) , ('d',4) , ('d',5) , ('d',6) , ('d',7) , ('d',8),
                ('e',1) , ('e',2) , ('e',3) , ('e',4) , ('e',5) , ('e',6) , ('e',7) , ('e',8),
                ('f',1) , ('f',2) , ('f',3) , ('f',4) , ('f',5) , ('f',6) , ('f',7) , ('f',8),
                ('g',1) , ('g',2) , ('g',3) , ('g',4) , ('g',5) , ('g',6) , ('g',7) , ('g',8),
                ('h',1) , ('h',2) , ('h',3) , ('h',4) , ('h',5) , ('h',6) , ('h',7) , ('h',8)]

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
visualizeBoard :: Board -> String
visualizeBoard (player, whitePieces, blackPieces) = "    a    b    c    d    e    f    g    h\n" ++ (concatMap (\rowNumber -> (getRow rowNumber (player, whitePieces, blackPieces)) ++ "\n") [8,7,6,5,4,3,2,1] ) ++ "\n" ++ "Turn : " ++ (playerString player)

playerString :: Player -> String
playerString White = "White"
playerString Black = "Black"

typestoString :: Piece -> (String, Location)
typestoString (P loc) = ("P", loc)
typestoString (N loc) = ("N", loc)
typestoString (K loc) = ("K", loc)
typestoString (Q loc) = ("Q", loc)
typestoString (R loc) = ("R", loc)
typestoString (B loc) = ("B", loc)

getPieceStr :: Location -> Board -> String
getPieceStr location (_,whitePieces,blackPieces)
  | searchRes (search location whitePieces) == "" && searchRes (search location blackPieces) /= "" = searchRes (search location blackPieces) ++ "B"
  | searchRes (search location whitePieces) /= "" && searchRes (search location blackPieces) == "" = searchRes (search location whitePieces) ++ "W"
  | otherwise = "  "

intString :: Int -> String
intString n | n==1 = "1"
            | n==2 = "2"
            | n==3 = "3"
            | n==4 = "4"
            | n==5 = "5"
            | n==6 = "6"
            | n==7 = "7"
            | n==8 = "8"

getRow :: Int -> Board -> String
getRow row board = (intString row) ++ " |" ++ concatMap (\location -> " " ++ (getPieceStr location board) ++ " |") (getLocPair row ['a','b','c','d','e','f','g','h'])


getLocPair :: Int -> [Char] -> [Location]
getLocPair rowNumber columnCharacters = [(col, rowNumber) | col <- columnCharacters]

search :: Location -> [Piece] -> [(String, Location)]
search location pieces = filter (\(str, loc) -> location == loc) (map typestoString pieces)

searchRes :: [(String, Location)] -> String
searchRes pieces = case pieces of
  [] -> ""
  (str, _) : _ -> str


----------
-- check the legability of moving given piece into the given location
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

checkWhere :: Piece -> Board -> Player
checkWhere p (player , white , black)   | elem p white = White
                                        | elem p black = Black

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
checkDiagonal (c1,r1) (c2,r2) board   | r1 > r2 && (c1) > ( c2) = checkDiagonalLeftDown 1 (c1,r1) (c2,r2) board
                                      | r1 > r2 && (c1) < ( c2) = checkDiagonalRightDown 1 (c1,r1) (c2,r2) board
                                      | r1 < r2 && (c1) < ( c2) = checkDiagonalRightUp 1 (c1,r1) (c2,r2) board
                                      | r1 < r2 && (c1) > ( c2) = checkDiagonalLeftUp 1 (c1,r1) (c2,r2) board
                                      | otherwise = False

checkDiagonalRightUp :: Int -> Location -> Location -> Board -> Bool
checkDiagonalRightUp i (c1 , r1) (c2,r2) board  | (getIndex (c1) +i) > 7 || (r1+i) > 8 = False
                                                | (isEmpty ((setIndex ((getIndex c1) + i)),(r1+i)) board) && (setIndex ((getIndex c1) + i)) ==c2 && (r1+i) == r2 = True
                                                | (isEmpty ((setIndex ((getIndex c1) + i)),(r1+i)) board) && not ((setIndex ((getIndex c1) + i)) ==c2 && (r1+i) == r2)  = checkDiagonalRightUp (i+1) (c1 , r1) (c2,r2) board
                                                | not (isEmpty ((setIndex ((getIndex c1) + i)),(r1+i)) board) &&  ( ((getIndex c1) + i)) ==(getIndex c2) && (r1+i) == r2 = True
                                                | otherwise = False

checkDiagonalRightDown :: Int -> Location -> Location -> Board -> Bool
checkDiagonalRightDown i (c1 , r1) (c2,r2) board  | (getIndex (c1) +i) > 7 || (r1-i) < 1 = False
                                                  | (isEmpty ((setIndex ((getIndex c1) + i)),(r1-i))  board) && (setIndex ((getIndex c1) + i)) ==c2 && (r1-i) == r2 = True
                                                  | (isEmpty ((setIndex ((getIndex c1) + i)),(r1-i))  board) && not ((setIndex ((getIndex c1) + i)) ==c2 && (r1-i) == r2)= checkDiagonalRightDown (i+1) (c1 , r1) (c2,r2) board
                                                  | not (isEmpty ((setIndex ((getIndex c1) + i)),(r1+i)) board) && ( ((getIndex c1) + i)) ==(getIndex c2) && (r1-i)==r2 = True
                                                  | otherwise = False

checkDiagonalLeftDown :: Int -> Location -> Location -> Board -> Bool
checkDiagonalLeftDown i (c1 , r1) (c2,r2) board   | (getIndex (c1) -i) < 0 || (r1-i) < 1 = False
                                                  | (isEmpty ((setIndex ((getIndex c1) - i)),(r1-i))  board) && (setIndex ((getIndex c1) - i)) ==c2 && (r1-i)== r2 = True
                                                  | (isEmpty ((setIndex ((getIndex c1) - i)),(r1-i))  board) && not((setIndex ((getIndex c1) - i)) ==c2 && (r1-i)== r2) = checkDiagonalLeftDown (i+1) (c1 , r1) (c2,r2) board
                                                  | not (isEmpty ((setIndex ((getIndex c1) - i)),(r1-i))  board) && ( ((getIndex c1) - i)) ==(getIndex c2) && (r1-i)== r2 = True
                                                  | otherwise = False

checkDiagonalLeftUp :: Int -> Location -> Location -> Board -> Bool
checkDiagonalLeftUp i (c1 , r1) (c2,r2) board     | (getIndex (c1) -i) < 0 || (r1+i) > 8 = False
                                                  | (isEmpty ((setIndex ((getIndex c1) - i)),(r1+i))  board) && (setIndex ((getIndex c1) - i)) ==c2 && (r1+i)== r2 = True
                                                  | (isEmpty ((setIndex ((getIndex c1) - i)),(r1+i))  board) && not ((setIndex ((getIndex c1) - i)) ==c2 && (r1+i)== r2) = checkDiagonalLeftUp (i+1) (c1 , r1) (c2,r2) board
                                                  | not (isEmpty ((setIndex ((getIndex c1) - i)),(r1+i))  board) && ( ((getIndex c1) - i)) ==(getIndex c2) && (r1+i)== r2 = True
                                                  | otherwise = False

isLegal :: Piece -> Board -> Location -> Bool

isLegal (R currLoc) board newLoc = rookLegalHelper  (R currLoc) board newLoc
isLegal (B currLoc) board newLoc = bishopLegalHelper (B currLoc) board newLoc
isLegal (Q currLoc) board newLoc = queenLegalHelper (Q currLoc) board newLoc
isLegal (K currLoc) board newLoc = kingLegalHelper (K currLoc) board newLoc
isLegal (N currLoc) board newLoc = knightLegalHelper (N currLoc) board newLoc
isLegal (P currLoc) board newLoc = pawnLegalHelper (P currLoc) board newLoc



rookLegalHelper :: Piece -> Board -> Location -> Bool
rookLegalHelper (R (c1,r1)) (player , white ,black) (c2,r2)     | c1/= c2 && r1/=r2 = False
                                                                | r1==r2 && c1 > c2 && (checkWhere (R (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2) && rookLocationColumn_Left (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | r1==r2 && c1 > c2 && (checkWhere (R (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) && rookLocationColumn_Left (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | r1==r2 && c1 < c2 && (checkWhere (R (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2) && rookLocationColumn_Right (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | r1==r2 && c1 < c2 && (checkWhere (R (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) && rookLocationColumn_Right (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | c1==c2 && r1 > r2 && (checkWhere (R (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2) && rookLocationRow_Down (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | c1==c2 && r1 > r2 && (checkWhere (R (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) && rookLocationRow_Down (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | c1==c2 && r1 < r2 && (checkWhere (R (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2) && rookLocationRow_Up (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | c1==c2 && r1 < r2 && (checkWhere (R (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) && rookLocationRow_Up (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | otherwise = False
                                                                                                                         
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
bishopLegalHelper (B (c1,r1)) (player , white ,black) (c2,r2)   | (checkWhere (B (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2)  && checkDiagonal (c1,r1) (c2,r2) (player , white ,black)
                                                                | (checkWhere (B (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)  && checkDiagonal (c1,r1) (c2,r2) (player , white ,black)


queenLegalHelper :: Piece -> Board -> Location  -> Bool
queenLegalHelper  (Q (c1,r1)) (player , white ,black) (c2,r2)   | r1==r2 && c1 > c2 && (checkWhere (Q (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2) && rookLocationColumn_Left (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | r1==r2 && c1 > c2 && (checkWhere (Q (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) && rookLocationColumn_Left (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | r1==r2 && c1 < c2 && (checkWhere (Q (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2) && rookLocationColumn_Right (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | r1==r2 && c1 < c2 && (checkWhere (Q (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) && rookLocationColumn_Right (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | c1==c2 && r1 > r2 && (checkWhere (Q (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2) && rookLocationRow_Down (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | c1==c2 && r1 > r2 && (checkWhere (Q (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) && rookLocationRow_Down (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | c1==c2 && r1 < r2 && (checkWhere (Q (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2) && rookLocationRow_Up (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | c1==c2 && r1 < r2 && (checkWhere (Q (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) && rookLocationRow_Up (R (c1,r1)) (player , white ,black) (c2,r2)
                                                                | (checkWhere (Q (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2)  && checkDiagonal (c1,r1) (c2,r2) (player , white ,black)
                                                                | (checkWhere (Q (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)  && checkDiagonal (c1,r1) (c2,r2) (player , white ,black)

kingLegalHelper :: Piece -> Board -> Location  -> Bool
kingLegalHelper (K (c1,r1)) (player , white ,black) (c2,r2) | r1==r2 && ((getIndex c1) -1) ==(getIndex c2) && (checkWhere (K (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                            | r1==r2 && ((getIndex c1) -1) ==(getIndex c2) && (checkWhere (K (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) 
                                                            | r1==r2 && ((getIndex c1) +1) ==(getIndex c2) && (checkWhere (K (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                            | r1==r2 && ((getIndex c1) +1) ==(getIndex c2) && (checkWhere (K (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2)
                                                            | c1==c2 && r1+1 == r2 && (checkWhere (K (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                            | c1==c2 && r1+1 == r2 && (checkWhere (K (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) 
                                                            | c1==c2 && r1-1 == r2 && (checkWhere (K (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                            | c1==c2 && r1-1 == r2 && (checkWhere (K (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2)
                                                            | ((getIndex c1) -1) ==(getIndex c2) && r1-1 == r2 && (checkWhere (K (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                            | ((getIndex c1) -1) ==(getIndex c2) && r1-1 == r2 && (checkWhere (K (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) 
                                                            | ((getIndex c1) +1) ==(getIndex c2) && r1-1 == r2 && (checkWhere (K (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                            | ((getIndex c1) +1) ==(getIndex c2) && r1-1 == r2 && (checkWhere (K (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) 
                                                            | ((getIndex c1) -1) ==(getIndex c2) && r1+1 == r2 && (checkWhere (K (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                            | ((getIndex c1) -1) ==(getIndex c2) && r1+1 == r2 && (checkWhere (K (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) 
                                                            | ((getIndex c1) +1) ==(getIndex c2) && r1+1 == r2 && (checkWhere (K (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                            | ((getIndex c1) +1) ==(getIndex c2) && r1+1 == r2 && (checkWhere (K (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2) 
                                                            | otherwise = False

knightLegalHelper :: Piece -> Board -> Location  -> Bool
knightLegalHelper (N (c1,r1)) (player , white ,black) (c2,r2)   | (getIndex c1)+1 == (getIndex c2) && r1+2==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                                | (getIndex c1)+1 == (getIndex c2) && r1+2==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2)
                                                                | (getIndex c1)-1 == (getIndex c2) && r1+2==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                                | (getIndex c1)-1 == (getIndex c2) && r1+2==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2)
                                                                | (getIndex c1)+1 == (getIndex c2) && r1-2==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                                | (getIndex c1)+1 == (getIndex c2) && r1-2==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2)
                                                                | (getIndex c1)-1 == (getIndex c2) && r1-2==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                                | (getIndex c1)-1 == (getIndex c2) && r1-2==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2)
                                                                | (getIndex c1)+2 == (getIndex c2) && r1+1==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                                | (getIndex c1)+2 == (getIndex c2) && r1+1==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2)
                                                                | (getIndex c1)-2 == (getIndex c2) && r1+1==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                                | (getIndex c1)-2 == (getIndex c2) && r1+1==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2)
                                                                | (getIndex c1)+2 == (getIndex c2) && r1-1==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                                | (getIndex c1)+2 == (getIndex c2) && r1-1==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2)
                                                                | (getIndex c1)-2 == (getIndex c2) && r1-1==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==Black = checkValid black (c2,r2)
                                                                | (getIndex c1)-2 == (getIndex c2) && r1-1==r2 && (checkWhere (N (c1,r1)) (player , white ,black))==White = checkValid white (c2,r2)
                                                                | otherwise = False

pawnLegalHelper :: Piece -> Board -> Location -> Bool
pawnLegalHelper (P (c1,r1)) (player , white ,black) (c2,r2) | ((checkWhere (P (c1,r1)) (player , white ,black))==White) && r1==8 = False
                                                            | ((checkWhere (P (c1,r1)) (player , white ,black))==Black) && r1==1 = False
                                                            | ((checkWhere (P (c1,r1)) (player , white ,black))==Black) && (getIndex c1)==(getIndex c2) && r1 == 7 && (r2== r1-1 || r2==r1-2) = (checkValid black (c2,r2)) && (((r2 ==r1-1) && isEmpty (c2,r2) (player , white ,black)) || (r2==r1-2 && isEmpty (c2,r1-1) (player , white ,black) && isEmpty (c2,r2) (player , white ,black)))
                                                            | ((checkWhere (P (c1,r1)) (player , white ,black))==White) && (getIndex c1)==(getIndex c2) && r1 == 2 && (r2== r1+1 || r2==r1+2) = (checkValid white (c2,r2)) && (((r2 ==r1+1) && isEmpty (c2,r2) (player , white ,black)) || (r2==r1+2 && isEmpty (c2,r1+1) (player , white ,black) && isEmpty (c2,r2) (player , white ,black)))
                                                            | ((checkWhere (P (c1,r1)) (player , white ,black))==Black) && (getIndex c1)==(getIndex c2) && r2 == r1-1 = (checkValid black (c2,r2)) && isEmpty (c2,r2) (player , white ,black)
                                                            | ((checkWhere (P (c1,r1)) (player , white ,black))==White) && (getIndex c1)==(getIndex c2) && r2 == r1+1 = (checkValid white (c2,r2)) && isEmpty (c2,r2) (player , white ,black)
                                                            | ((checkWhere (P (c1,r1)) (player , white ,black))==Black) && (r2 ==  r1-1) && (((getIndex c2)==((getIndex c1)-1))||((getIndex c2)==((getIndex c1)+1))) && not (isEmpty (c2,r2) (player , white ,black)) = checkValid black (c2,r2)
                                                            | ((checkWhere (P (c1,r1)) (player , white ,black))==White) && (r2 ==  r1+1) && (((getIndex c2)==((getIndex c1)-1))||((getIndex c2)==((getIndex c1)+1))) && not (isEmpty (c2,r2) (player , white ,black)) = checkValid white (c2,r2)
                                                            | otherwise = False
----------
-- retuen list of legal valid move for given piece
suggestMove :: Piece -> Board -> [Location]
suggestMove piece board = suggestMoveHelper piece board allLocations

suggestMoveHelper :: Piece -> Board -> [Location] -> [Location]
suggestMoveHelper _ _ [] = []
suggestMoveHelper piece board (h:t)  | isLegal piece board h = h:(suggestMoveHelper piece board t)
                                     | otherwise = (suggestMoveHelper piece board t)

----------
-- return the board after moving the given piece 
move :: Piece -> Location -> Board -> Board 
move piece location (player,white,black)| not (isLegal piece (player,white,black) location) = error ("illegal move for piece "++(show piece))
                                        | not (checkTurn piece (player,white,black)) = error ("It's "++(show player)++" player's turn, "++(show (flipColor player))++" can't move")
                                        | player == White = (Black,updateList white piece location,cheackEat black location)
                                        | player == Black = (White,cheackEat white location,updateList black piece location)

updateList :: [Piece] -> Piece -> Location -> [Piece]
updateList list piece location = (updatePiece piece location):(filter (/= piece) list)

cheackEat :: [Piece] -> Location -> [Piece]
cheackEat list location = filter (\x -> (getLocation x) /= location) list

checkTurn :: Piece -> Board -> Bool
checkTurn piece (player,white,black)    | player==White && elem piece black = False
                                        | player==Black && elem piece white = False
                                        | otherwise = True

updatePiece :: Piece -> Location -> Piece
updatePiece (P l) location = P location
updatePiece (N l) location = N location
updatePiece (K l) location = K location
updatePiece (Q l) location = Q location
updatePiece (R l) location = R location
updatePiece (B l) location = B location

flipColor :: Player -> Player
flipColor White = Black
flipColor Black = White
----------
-- for easier testing
similarB :: Board -> Board -> Bool 
similarB (p1,w1,b1) (p2,w2,b2) = (p1 == p2) && (allE w1 w2) && (allE w2 w1) && (allE b1 b2) && (allE b2 b1)

allE :: [Piece] -> [Piece] -> Bool 
allE [] _ = True
allE (h:t) l = (elem h l) && allE t l
----------