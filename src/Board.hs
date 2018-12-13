module Board
  (
    Board, Rank, Square
  , Piece (Piece)
  , Piecetype (Pawn, Knight, Bishop, Rook, King, Queen)
  , Pawntype (Starting, Passant, Normal)
  , Color (White, Black)
  , initialBoard
  ) where

type Board = [Rank]
type Rank = [Square]
type Square = Maybe Piece
data Piece = Piece Piecetype Color deriving Show
data Piecetype = Pawn Pawntype | Knight | Bishop | Rook | King | Queen deriving (Show, Eq)
data Pawntype = Starting | Passant | Normal deriving (Show, Eq)
data Color = White | Black deriving (Show, Eq)

initialBoard = [(rCloser Black), (pawnRow Black)] ++ (replicate 4 emptyRow) ++ [(pawnRow White), (rCloser White)]
  where rCloserl :: Color -> Rank
        rCloserl c = map Just [Piece Rook c, Piece Knight c, Piece Bishop c]

        rCloser :: Color -> Rank
        rCloser c = (rCloserl c) ++ map Just [Piece Queen c, Piece King c] ++ (reverse $ rCloserl c)
  
        pawnRow :: Color -> Rank
        pawnRow c = replicate 8 (Just (Piece (Pawn Starting) c))

        emptyRow :: Rank
        emptyRow = replicate 8 Nothing

