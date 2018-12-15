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
        mml = [miniMax c (n-1) False b' (other c) | b' <- bs]

evalB b c | c == White = evaluateBoard b
          | c == Black = -(evaluateBoard b)

miniMax :: Color -> Int -> Bool -> Board -> Color -> Int
miniMax trueC 0 _ b c = evalB b trueC

-- cada elemento de list' tem um ramo. o elemento de maior(ou menor) eval eh o
-- RAMO que devo escolher
miniMax trueC n True b c = case newMoves of [] -> evalB b trueC
                                            l -> maximum list'
  where newMoves = possible_moves b c
        mm try = miniMax trueC (n-1) False try (other c)
        list' = [mm try | try <- newMoves]
 
miniMax trueC n False b c = case newMoves of [] -> evalB b trueC
                                             l -> minimum list'
  where newMoves = possible_moves b c
        mm try = miniMax trueC (n-1) True try (other c)
        list' = [mm try | try <- newMoves]
