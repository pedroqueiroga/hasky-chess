module Board
  ( Board, Rank, Square, SquarePos
  , Piece (Piece)
  , Piecetype (Pawn, Knight, Bishop, Rook, King, Queen)
  , Pawntype (Starting, Passant, MakeMePassant, Normal)
  , RKType (Unmov, Moved)
  , Color (White, Black)
  , initialBoard
  , getSquare
  , isPiece
  , isPiecePos
  , isPieceColor
  , isPieceColorPos
  , getKing
  , rlxEqPcs
  , getPiecePositions
  , other
  ) where

import Data.List (findIndex)
import Data.Maybe (fromJust)

type Board = [Rank]
type Rank = [Square]
type Square = Maybe Piece
type SquarePos = (Int, Int)
data Piece = Piece Piecetype Color deriving (Show, Eq)
data Piecetype = Pawn Pawntype | Knight | Bishop | Rook RKType | King RKType | Queen deriving (Show, Eq)
data Pawntype = Starting | Passant | MakeMePassant| Normal deriving (Show, Eq)
data RKType = Unmov | Moved deriving (Show, Eq)
data Color = White | Black deriving (Show, Eq)

class Other a where
  other :: a -> a

instance Other Color where
  other White = Black
  other Black = White
  
initialBoard = [(rCloser White), (pawnRow White)] ++ (replicate 4 emptyRow) ++ [(pawnRow Black), (rCloser Black)]
  where rCloserl :: Color -> Rank
        rCloserl c = map Just [Piece (Rook Unmov) c, Piece Knight c, Piece Bishop c]

        rCloser :: Color -> Rank
        rCloser c = (rCloserl c) ++ map Just [Piece Queen c, Piece (King Unmov) c] ++ (reverse $ rCloserl c)

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
isPieceColor Nothing _ = False
isPieceColor (Just (Piece _ c')) c = c' == c

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
      where f = if (fi1) == Nothing then findIndex ((==) (Just (Piece (King Moved) c))) x else fi1
              where fi1 = findIndex ((==) (Just (Piece (King Unmov) c))) x

    getKing' _ [] = Nothing

rlxEqPcs :: Square -> Square -> Bool
rlxEqPcs (Just (Piece (Pawn _) c)) (Just (Piece (Pawn _) c')) = c == c'
rlxEqPcs (Just (Piece (Rook _) c)) (Just (Piece (Rook _) c')) = c == c'
rlxEqPcs (Just (Piece (King _) c)) (Just (Piece (King _) c')) = c == c'
rlxEqPcs p p' = p == p'

getPiecePositions :: Board -> Square -> [SquarePos]
getPiecePositions b sq = [(i,j) | rank <- b
                                , i <- [0..length b - 1]
                                , j <- [0..length b - 1]
                                , rlxEqPcs (getSquare (i,j) b) sq]
