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
isLegal :: Piece -> Board -> Location -> Bool
isLegal 




----------
-- retuen list of illegal valid move for given piece
suggestMove :: Piece -> Board -> [Location]




----------
-- return the board after moving the given piece 
move :: Piece -> Location -> Board -> Board 