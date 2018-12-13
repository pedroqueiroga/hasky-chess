module EvaluateBoard
  ( evaluateBoard
  , initialBoard
  ) where

type Board = [Rank]
type Rank = [Square]
type Square = Maybe Piece
data Piece = Piece Piecetype Color deriving Show
data Piecetype = Pawn Pawntype | Knight | Bishop | Rook | King | Queen deriving (Show, Eq)
data Pawntype = Starting | Passant | Normal deriving (Show, Eq)
data Color = White | Black deriving (Show, Eq)

piecetypeValue :: Piecetype -> Int
piecetypeValue (Pawn _) = 10
piecetypeValue Knight = 30
piecetypeValue Bishop = 31
piecetypeValue Rook = 50
piecetypeValue Queen = 90
piecetypeValue King = 900

pieceValue :: Piece -> Int
pieceValue (Piece p White) = piecetypeValue p
pieceValue (Piece p Black) = -(piecetypeValue p)

squareValue :: Square -> Int
squareValue (Just p) = pieceValue p
squareValue Nothing = 0

evaluateRow :: Rank -> Int
evaluateRow rank = foldr1 (+) (map squareValue rank)

evaluateBoard :: Board -> Int
evaluateBoard board = foldr1 (+) (map evaluateRow board)

rCloserl :: Color -> Rank
rCloserl c = map Just [Piece Rook c, Piece Knight c, Piece Bishop c]

rCloser :: Color -> Rank
rCloser c = (rCloserl c) ++ map Just [Piece Queen c, Piece King c] ++ (reverse $ rCloserl c)

pawnRow :: Color -> Rank
pawnRow c = replicate 8 (Just (Piece (Pawn Starting) c))

emptyRow :: Rank
emptyRow = replicate 8 Nothing

initialBoard = [(rCloser Black), (pawnRow Black)] ++ (replicate 4 emptyRow) ++ [(pawnRow White), (rCloser White)]
