module BestMove (bestMove) where

import Board
import EvaluateBoard
import MoveMaker

import Data.Maybe (fromJust)
import Data.List (elemIndex)

bestMove :: Int -> Board -> Color -> Board
bestMove n b c
  | n < 0 = b
  | otherwise = bs !! (fromJust $ elemIndex (maximum mml) mml)
  where bs = possible_moves b c
        mml = [miniMax n True b' c | b' <- bs]

evalB b c | c == White = evaluateBoard b
          | c == Black = -(evaluateBoard b)

miniMax :: Int -> Bool -> Board -> Color -> Int
miniMax 0 _ b c = evalB b c

-- cada elemento de list' tem um ramo. o elemento de maior(ou menor) eval eh o
-- RAMO que devo escolher
miniMax n True b c = maximum list' 
  where newMoves = possible_moves b c
        mm try = miniMax (n-1) False try (other c)
        list' = [if ebc > mm try then ebc else mm try | try <- newMoves]
        ebc = evalB b c
 
miniMax n False b c = minimum list'
  where newMoves = possible_moves b c
        mm try = miniMax (n-1) True try (other c)
        list' = [if ebc < mm try then ebc else mm try | try <- newMoves]
        ebc = evalB b c
