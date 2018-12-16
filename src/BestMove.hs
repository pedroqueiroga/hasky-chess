module BestMove (bestMove) where

import Board
import EvaluateBoard
import MoveMaker

import Data.Maybe (fromJust)
import Data.List (elemIndex)

bestMove :: Int -> Board -> Color -> Board
bestMove n b c
  | n < 0 = b
  | otherwise = bs !! (fst $ forEach 0 (0, (-99999999)) (-99999999) 99999999 bs)
  where bs = possible_moves b c
        mml = [miniMax c (n-1) False b' (other c) (-99999999) 99999999 | b' <- bs]
        mm try alpha beta = miniMax c (n-1) False try (other c) alpha beta
        -- aqui eu vou checar meus moves, querendo saber apenas o indice
        -- do melhor dentre todos os possiveis
        forEach idx (maxidx, value) alpha beta (x:xs) = if maxav >= beta then maxvmm else forEach (idx+1) maxvmm maxav beta xs
          where maxvmm = if mm' > value
                         then (idx, mm')
                         else (maxidx, value)
                maxav  = max alpha (snd maxvmm)
                mm' = mm x alpha beta
        forEach idx (maxidx, value) alpha beta [] = (maxidx, value)


evalB b c | c == White = evaluateBoard b
          | c == Black = -(evaluateBoard b)

miniMax :: Color -> Int -> Bool -> Board -> Color -> Int -> Int -> Int
miniMax trueC 0 _ b c _ _ = evalB b trueC

-- cada elemento de list' tem um ramo. o elemento de maior(ou menor) eval eh o
-- RAMO que devo escolher
-- esses [] denotam chequemate, que tem o valor da board menos o do rei perdido
-- se for cheque-mate seu, entao a board do inimigo perde o valor do rei (ou a sua board "ganha um rei")
-- TODO possivel: desisto, checando se todas as opcoes sao sem rei
-- se for vc levando cheque-mate, entao a sua board perde o valor do rei
miniMax trueC n True b c alpha beta = case newMoves of [] -> (evalB b trueC) - (piecetypeValue (King Moved))
                                                       l -> forEach (-99999999) alpha beta newMoves
  where newMoves = possible_moves b c
        mm try alpha' beta' = miniMax trueC (n-1) False try (other c) alpha' beta'
        list' = [mm try | try <- newMoves]
        forEach :: Int -> Int -> Int -> [Board] -> Int
        forEach value alpha' beta (x:xs) = if maxav >= beta then maxvmm else forEach maxvmm maxav beta xs
          where maxvmm = max value (mm x alpha' beta)
                maxav  = max alpha' maxvmm
        forEach value alpha' beta [] = value
 
miniMax trueC n False b c alpha beta = case newMoves of [] -> (evalB b trueC) + (piecetypeValue (King Moved))
                                                        l -> forEach (99999999) alpha beta newMoves
  where newMoves = possible_moves b c
        mm try alpha' beta' = miniMax trueC (n-1) True try (other c) alpha' beta'
        list' = [mm try | try <- newMoves]
        forEach :: Int -> Int -> Int -> [Board] -> Int
        forEach value alpha beta' (x:xs) = if alpha >= minbv then minvmm else forEach minvmm alpha minbv xs
          where minvmm = min value (mm x alpha beta')
                minbv  = min beta minvmm
        forEach value alpha beta' [] = value
