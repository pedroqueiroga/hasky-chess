module Board
  ( Board, Rank, Square, SquarePos
  , Piece (Piece)
  , Piecetype (Pawn, Knight, Bishop, Rook, King, Queen)
  , Pawntype (Starting, Passant, MakeMePassant, Normal)
  , Color (White, Black)
  , initialBoard
  , getSquare
  , isPiece
  , isPiecePos
  , isPieceColor
  , isPieceColorPos
  , getKing
  ) where

import Data.List (findIndex)
import Data.Maybe (fromJust)

type Board = [Rank]
type Rank = [Square]
type Square = Maybe Piece
type SquarePos = (Int, Int)
data Piece = Piece Piecetype Color deriving (Show, Eq)
data Piecetype = Pawn Pawntype | Knight | Bishop | Rook | King | Queen deriving (Show, Eq)
data Pawntype = Starting | Passant | MakeMePassant| Normal deriving (Show, Eq)
data Color = White | Black deriving (Show, Eq)

initialBoard = [(rCloser White), (pawnRow White)] ++ (replicate 4 emptyRow) ++ [(pawnRow Black), (rCloser Black)]
  where rCloserl :: Color -> Rank
        rCloserl c = map Just [Piece Rook c, Piece Knight c, Piece Bishop c]

        rCloser :: Color -> Rank
        rCloser c = (rCloserl c) ++ map Just [Piece Queen c, Piece King c] ++ (reverse $ rCloserl c)

        pawnRow :: Color -> Rank
        pawnRow c = replicate 8 (Just (Piece (Pawn Starting) c))

        emptyRow :: Rank
        emptyRow = replicate 8 Nothing

getSquare :: SquarePos -> Board -> Square
getSquare (i, j) b = b !! i !! j

isPiece :: Square -> Bool
isPiece Nothing = False
isPiece _ = True

isPieceColor :: Square -> Color -> Bool
isPieceColor sq@(Just (Piece p c')) c = isPiece sq && c' == c

isPiecePos :: SquarePos -> Board -> Bool
isPiecePos sqp = isPiece . getSquare sqp

isPieceColorPos :: SquarePos -> Board -> Color -> Bool
isPieceColorPos sqp = isPieceColor . (getSquare sqp)

getKing :: Color -> Board -> Maybe SquarePos
getKing c b = getKing' 0 b
  where
    getKing' :: Int -> Board -> Maybe (Int, Int)
    getKing' n (x:xs) | f /= Nothing = Just (n, fromJust $ f)
                      | otherwise = getKing' (n+1) xs
      where f = findIndex ((==) (Just (Piece King c))) x

    getKing' _ [] = Nothing
