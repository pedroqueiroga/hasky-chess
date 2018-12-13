module EvaluateBoard
  ( evaluateBoard
  ) where

import Board

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

