module EvaluateBoard
  ( evaluateBoard
  ) where

import Board

piecetypeValue :: Piecetype -> Int
piecetypeValue (Pawn _) = 100
piecetypeValue Knight = 320
piecetypeValue Bishop = 330
piecetypeValue Rook = 500
piecetypeValue Queen = 900
piecetypeValue King = 20000

pieceSquareTable :: Piecetype -> [[Int]]
pieceSquareTable (Pawn _) = [[0,  0,  0,  0,  0,  0,  0,  0],
                             [50, 50, 50, 50, 50, 50, 50, 50],
                             [10, 10, 20, 30, 30, 20, 10, 10],
                             [5,  5, 10, 25, 25, 10,  5,  5],
                             [0,  0,  0, 20, 20,  0,  0,  0],
                             [5, -5,-10,  0,  0,-10, -5,  5],
                             [5, 10, 10,-20,-20, 10, 10,  5],
                             [0,  0,  0,  0,  0,  0,  0,  0]]

pieceSquareTable Knight = [[-50,-40,-30,-30,-30,-30,-40,-50],
                           [-40,-20,  0,  0,  0,  0,-20,-40],
                           [-30,  0, 10, 15, 15, 10,  0,-30],
                           [-30,  5, 15, 20, 20, 15,  5,-30],
                           [-30,  0, 15, 20, 20, 15,  0,-30],
                           [-30,  5, 10, 15, 15, 10,  5,-30],
                           [-40,-20,  0,  5,  5,  0,-20,-40],
                           [-50,-40,-30,-30,-30,-30,-40,-50]]

pieceSquareTable Bishop = [[-20,-10,-10,-10,-10,-10,-10,-20],
                           [-10,  0,  0,  0,  0,  0,  0,-10],
                           [-10,  0,  5, 10, 10,  5,  0,-10],
                           [-10,  5,  5, 10, 10,  5,  5,-10],
                           [-10,  0, 10, 10, 10, 10,  0,-10],
                           [-10, 10, 10, 10, 10, 10, 10,-10],
                           [-10,  5,  0,  0,  0,  0,  5,-10],
                           [-20,-10,-10,-10,-10,-10,-10,-20]]

pieceSquareTable Rook = [[0,  0,  0,  0,  0,  0,  0,  0],
                         [5, 10, 10, 10, 10, 10, 10,  5],
                         [-5,  0,  0,  0,  0,  0,  0, -5],
                         [-5,  0,  0,  0,  0,  0,  0, -5],
                         [-5,  0,  0,  0,  0,  0,  0, -5],
                         [-5,  0,  0,  0,  0,  0,  0, -5],
                         [-5,  0,  0,  0,  0,  0,  0, -5],
                         [0,  0,  0,  5,  5,  0,  0,  0]]
  
pieceSquareTable Queen = [[-20,-10,-10, -5, -5,-10,-10,-20],
                          [-10,  0,  0,  0,  0,  0,  0,-10],
                          [-10,  0,  5,  5,  5,  5,  0,-10],
                          [-5,  0,  5,  5,  5,  5,  0, -5],
                          [0,  0,  5,  5,  5,  5,  0, -5],
                          [-10,  5,  5,  5,  5,  5,  0,-10],
                          [-10,  0,  5,  0,  0,  0,  0,-10],
                          [-20,-10,-10, -5, -5,-10,-10,-20]]

pieceSquareTable King = [[-30,-40,-40,-50,-50,-40,-40,-30],
                         [-30,-40,-40,-50,-50,-40,-40,-30],
                         [-30,-40,-40,-50,-50,-40,-40,-30],
                         [-30,-40,-40,-50,-50,-40,-40,-30],
                         [-20,-30,-30,-40,-40,-30,-30,-20],
                         [-10,-20,-20,-20,-20,-20,-20,-10],
                         [20, 20,  0,  0,  0,  0, 20, 20],
                         [20, 30, 10,  0,  0, 10, 30, 20]]

pieceTableValue :: Piece -> [Int]
pieceTableValue (Piece p White) = concat (pieceSquareTable p)
pieceTableValue (Piece p Black) = map (*(-1)) (concat (reverse (pieceSquareTable p)))

pieceSquareTableValue :: Int -> Square -> Int
pieceSquareTableValue i (Just p) =
  pieceValue p + ((pieceTableValue p) !! i)
pieceSquareTableValue _ Nothing = 0

pieceValue :: Piece -> Int
pieceValue (Piece p White) = piecetypeValue p
pieceValue (Piece p Black) = -(piecetypeValue p)

squareValue :: Square -> Int
squareValue (Just p) = pieceValue p
squareValue Nothing = 0

evaluateRow :: Rank -> Int
evaluateRow rank = foldl1 (+) (map squareValue rank)

{-
evaluateBoard :: Board -> Int
evaluateBoard board = foldl1 (+) (map evaluateRow board)
-}
evaluateBoard :: Board -> Int
evaluateBoard board = ev' 0 (concat board)
  where
    ev' :: Int -> [Square] -> Int
    ev' i [p] = pieceSquareTableValue i p
    ev' i (p:xs) = (pieceSquareTableValue i p) + (ev' (i+1) xs)

{-
squareValueColor :: Color -> Square -> Int
squareValueColor c (Just p@(Piece p' c')) = if (c == c') then pieceValue p else 0
squareValueColor _ Nothing = 0

evaluateRowColor :: Color -> Rank -> Int
evaluateRowColor c rank = foldl1 (+) (map (squareValueColor c) rank)

evaluateBoardColor :: Color -> Board -> Int
evaluateBoardColor c board = -(foldl1 (+) (map (evaluateRowColor (other c)) board))
-}
