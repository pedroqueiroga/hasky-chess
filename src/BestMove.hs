module BestMove (bestMove) where

import Board
import EvaluateBoard
import MoveMaker

import Data.Maybe (fromJust)
import Data.List (elemIndex, sortBy)
import Data.Tree
import Data.Function (on)

bestMove :: Int -> State -> Board
bestMove n state@(b, c) = bestBoard
  where bs = possible_moves state
        evals = map (minimize n c) bs
        (bestBoard,_) =
          fst $ head $ sortBy (flip compare `on` snd) $ zip bs evals

        
-- bestMove n b c
--   | n < 0 = b
--   | otherwise = bs !! (fst $ forEach 0 (0, (-99999999)) (-99999999) 99999999 bs)
--   where bs = possible_moves c b
--         mml = [minimize c (n-1) b' (other c) (-99999999) 99999999 | b' <- bs]
--         mm try alpha beta = minimize c (n-1) try (other c) alpha beta
--         -- aqui eu vou checar meus moves, querendo saber apenas o indice
--         -- do melhor dentre todos os possiveis
--         forEach idx (maxidx, value) alpha beta (x:xs) = if maxav >= beta then maxvmm else forEach (idx+1) maxvmm maxav beta xs
--           where maxvmm = if mm' > value
--                          then (idx, mm')
--                          else (maxidx, value)
--                 maxav  = max alpha (snd maxvmm)
--                 mm' = mm x alpha beta
--         forEach idx (maxidx, value) alpha beta [] = (maxidx, value)

evalB :: Color -> State -> Int
evalB White (b,_) = evaluateBoard b
evalB Black (b,_) = -(evaluateBoard b)

reptree f a = Node a (map (reptree f) (f a))

gametree = reptree possible_moves

maximize' (Node n []) = n : []
maximize' (Node n sub) = mapmin (map minimize' sub)
  where mapmin (nums:rest) = (minimum nums):(omit (minimum nums) rest)

minimize' (Node n []) = n : []
minimize' (Node n sub) = mapmax (map maximize' sub)
  where mapmax (nums:rest) = (maximum nums):(omit (maximum nums) rest)

omit pot [] = []
omit pot (nums:rest) =
  if minleq nums pot
  then omit pot rest
  else (minimum nums):(omit (minimum nums) rest)

minleq [] pot = False
minleq (n:rest) pot = n <= pot || minleq rest pot

prune 0 (Node a x) = Node a []
prune n (Node a x) = Node a (map (prune $ n-1) x)

highfirst :: Tree Int -> Tree Int
highfirst (Node n sub) = Node n (sortBy (flip higher) (map lowfirst sub))
lowfirst (Node n sub) = Node n (sortBy (higher) (map highfirst sub))

higher (Node n1 _) (Node n2 _) = compare n1 n2

processed n c hl = (taketree 3 . hl) . fmap (evalB c) . prune n . gametree

maximize n c =
  maximum . maximize' . (processed n c highfirst)

minimize n c =
  minimum . minimize' . (processed n c lowfirst)

foldtree f g a (Node label subtrees) =
  f label (foldtrees f g a subtrees)
foldtrees f g a (subtree:rest) =
  g (foldtree f g a subtree) (foldtrees f g a rest)
foldtrees _ _ a [] = a

taketree n = foldtree (nodett n) (:) []

nodett n label sub = Node label (take n sub)



-- cada elemento de list' tem um ramo. o elemento de maior(ou menor) eval eh o
-- RAMO que devo escolher
-- esses [] denotam chequemate, que tem o valor da board menos o do rei perdido
-- se for cheque-mate seu, entao a board do inimigo perde o valor do rei (ou a sua board "ganha um rei")
-- TODO possivel: desisto, checando se todas as opcoes sao sem rei
-- se for vc levando cheque-mate, entao a sua board perde o valor do rei
-- maximize :: Color -> Int -> Board -> Color -> Int -> Int -> Int
-- maximize trueC 0 b _ _  _ = evalB b trueC
-- maximize trueC n b c alpha beta =
--   case newMoves of [] -> (evalB b trueC) - (piecetypeValue (King Moved))
--                    l -> forEach (-99999999) alpha beta newMoves
--   where newMoves = possible_moves c b
--         mm try = minimize trueC (n-1) try (other c)
--         forEach :: Int -> Int -> Int -> [Board] -> Int
--         forEach value _ _ [] = value
--         forEach value alpha' beta (x:xs) =
--           if maxav >= beta then maxvmm else forEach maxvmm maxav beta xs
--           where maxvmm = max value (mm x alpha' beta)
--                 maxav  = max alpha' maxvmm
 
-- minimize :: Color -> Int -> Board -> Color -> Int -> Int -> Int
-- minimize trueC 0 b _ _  _ = evalB b trueC
-- minimize trueC n b c alpha beta =
--   case newMoves of [] -> (evalB b trueC) + (piecetypeValue (King Moved))
--                    l -> forEach (99999999) alpha beta newMoves
--   where newMoves = possible_moves c b
--         mm try = maximize trueC (n-1) try (other c)
--         forEach :: Int -> Int -> Int -> [Board] -> Int
--         forEach value _ _ [] = value
--         forEach value alpha beta' (x:xs) =
--           if alpha >= minbv then minvmm else forEach minvmm alpha minbv xs
--           where minvmm = min value (mm x alpha beta')
--                 minbv  = min beta minvmm
