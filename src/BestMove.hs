module BestMove (bestMove) where

import Board
import EvaluateBoard

import Data.Maybe (fromJust)
import Data.List (findIndex)

bestMove :: [Board] -> Color -> Board
bestMove [] _ = initialBoard
bestMove bs c = bs !! (fromJust $ findIndex ((==) (foldl1 max evalBs)) evalBs)
  where evalBs | c == White = map evaluateBoard bs
               | otherwise = map ((*(-1)) . (evaluateBoard)) bs
