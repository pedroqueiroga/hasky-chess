module Main where

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Game as Game
import Graphics.Gloss.Interface.Pure.Game hiding (Color)
import Board
import BestMove
import MoveMaker
import System.IO.Unsafe
import Data.Maybe (fromJust)
import qualified Data.Set as Set

width, height, offset :: Int
width = 800
height = 600
offset = 0

whitePawn :: Gloss.Picture
whitePawn = Game.png "pieces/plt60.png"

blackPawn :: Gloss.Picture
blackPawn = Game.png "pieces/pdt60.png"

whiteRook :: Gloss.Picture
whiteRook = Game.png "pieces/rlt60.png"

blackRook :: Gloss.Picture
blackRook = Game.png "pieces/rdt60.png"

whiteBishop :: Gloss.Picture
whiteBishop = Game.png "pieces/blt60.png"

blackBishop :: Gloss.Picture
blackBishop = Game.png "pieces/bdt60.png"

whiteKnight :: Gloss.Picture
whiteKnight = Game.png "pieces/nlt60.png"

blackKnight :: Gloss.Picture
blackKnight = Game.png "pieces/ndt60.png"

whiteQueen :: Gloss.Picture
whiteQueen = Game.png "pieces/qlt60.png"

blackQueen :: Gloss.Picture
blackQueen = Game.png "pieces/qdt60.png"

whiteKing :: Gloss.Picture
whiteKing = Game.png "pieces/klt60.png"

blackKing :: Gloss.Picture
blackKing = Game.png "pieces/kdt60.png"

window :: Gloss.Display
window = Gloss.InWindow
  "hasky-chess"
  (width, height)
  (offset, offset)

backGround :: Gloss.Color
backGround = Gloss.black

fator = (width `div` 15) :: Int


mkBoard :: BoardState -> [Gloss.Picture]
mkBoard bs = [(Gloss.translate (fromIntegral (x)) (fromIntegral (y)) $ Gloss.color (if ((x `div` fator `mod` 2 == 1 && y `div` fator `mod` 2 == 1) || (x `div` fator `mod` 2 == 0 && y `div` fator `mod` 2 == 0))
                                                                                    then Gloss.greyN (0.5 - ifpp (y`div`fator - 1,x`div`fator - 1)) else Gloss.greyN (1 - ifpp (y`div`fator - 1 ,x`div`fator - 1))) $ Gloss.rectangleSolid (fromIntegral fator)  (fromIntegral fator)) | x <- (map (*fator) [1..8]), y <- (map (*fator) [1..8])]
  where
    pintandoPossivel (x,y) = elem (x,y) $ currentTips bs
    ifpp (x,y) = if currentPlayer bs == White && pintandoPossivel (x,y) then 0.2 else 0

mkPieces :: Board -> [Gloss.Picture]
mkPieces = mkPieces' (0,0)
  where mkPieces' :: SquarePos -> Board -> [Gloss.Picture]
        mkPieces' (i,j) b = Gloss.translate (fromIntegral ((j+1)*fator)) (fromIntegral ((i+1)*fator)) (mkPiece (getSquare (i,j) b)) : (if (i < 7) then mkPieces' (i+1, j) b else []) ++ (if (j < 7) then mkPieces' (i, j+1) b else [])
        mkPiece :: Square -> Gloss.Picture
        mkPiece (Just (Piece (Pawn _) c)) = if c == White then whitePawn else blackPawn
        mkPiece (Just (Piece (Rook _) c)) = if c == White then whiteRook else blackRook
        mkPiece (Just (Piece (Bishop) c)) = if c == White then whiteBishop else blackBishop
        mkPiece (Just (Piece (Knight) c)) = if c == White then whiteKnight else blackKnight
        mkPiece (Just (Piece (Queen) c)) = if c == White then whiteQueen else blackQueen
        mkPiece (Just (Piece (King _) c)) = if c == White then whiteKing else blackKing
        mkPiece Nothing = Gloss.blank

data BoardState =
  Game
  {
    board :: Board
  , currentPlayer :: Color
  , squareSelected :: Maybe SquarePos
  , currentMousePos :: Maybe SquarePos
  , lastMousePos :: Maybe SquarePos
  , currentTips :: [SquarePos]
  }

renderGame :: BoardState -> Gloss.Picture
renderGame game = drawBoard (board game)
  where drawBoard b = Gloss.translate (-4.5*(fromIntegral fator)) (-4.5*(fromIntegral fator)) $ Gloss.pictures $ ((mkBoard game) ++ (mkPieces b))

initialState :: BoardState
initialState = Game
  {
    board = initialBoard
  , currentPlayer = White
  , squareSelected = Nothing
  , currentMousePos = Nothing
  , lastMousePos = Nothing
  , currentTips = []
  }

stepGame :: BoardState -> BoardState
stepGame bs = bs { board = newBoard, currentPlayer = (other c), currentTips = possiblePositions bs }
  where newBoard = bestMove maxDepth (board bs) c
        c = currentPlayer bs

maxDepth :: Int
maxDepth = 3

playGame :: Float -> BoardState -> BoardState
playGame _ bs = if currentPlayer bs == Black then stepGame bs else bs

handleEvent :: Event -> BoardState -> BoardState
handleEvent (EventKey (MouseButton LeftButton) Down _ mp@(x,y)) bs
  = if (squareSelected bs) == Nothing
    then bs { squareSelected = Just (fromPixel mp), currentTips = possiblePositions bs }
    else if mb == Nothing
         then bs { squareSelected = Nothing, currentTips = possiblePositions bs { squareSelected = Nothing } }
         else bs { squareSelected = Nothing, board = fromJust mb, currentPlayer = other (currentPlayer bs), currentTips = [] }
  where mb = moveBoard (fromJust (squareSelected bs)) (fromPixel mp) (currentPlayer bs) (board bs)

handleEvent (EventMotion mp@(x,y)) bs =
  if lastMousePos bs /= curPos
  then bs { lastMousePos = currentMousePos bs, currentMousePos = curPos, currentTips = possiblePositions bs }
  else bs
  where bmp@(bmpx, bmpy) = fromPixel mp
        curPos = if bmpx > 7 || bmpx < 0 || bmpy > 7 || bmpy < 0 then Nothing else Just bmp

handleEvent (EventKey (MouseButton RightButton) Down _ mp@(x,y)) bs
  = bs { squareSelected = Nothing, currentTips = possiblePositions bs { squareSelected = Nothing } }

handleEvent _ bs = bs

-- | Traz de coordenadas de camera pra coordenadas de xadrez
-- WIDTH
fromPixelX :: Float -> Int
fromPixelX x = round $ (x + (fromIntegral fator * 7) / 2) / fromIntegral fator

-- | Traz de coordenadas de camera pra coordenadas de xadrez
-- HEIGHT
fromPixelY :: Float -> Int
fromPixelY y = round $ (y + (fromIntegral fator * 7) / 2) / fromIntegral fator
        
-- | Junta fromPixelW e fromPixelH
fromPixel :: (Float, Float) -> SquarePos
fromPixel (x, y) = (fromPixelX y , fromPixelY x)

moveBoard :: SquarePos -> SquarePos -> Color ->  Board -> Maybe Board
moveBoard sqp0@(i0, j0) sqp'@(i', j') c b
  | i0 < 0 || i0 > 7 || j0 < 0 || j0 > 7 || i' < 0 || i' > 7 || j' < 0 || j' > 7 = Nothing
  | otherwise =
  if (length pos_final == 1)
  then Just (pos_final !! 0)
  else Nothing
  where piece0 = getSquare sqp0 b
        pos0move = pos_move b piece0 sqp0 c
        pos_final = [x | x <- pos0move
                       , rlxEqPcs (x !! i' !! j') piece0
                       , x !! i0 !! j0 == Nothing]

possiblePositions :: BoardState -> [SquarePos]
possiblePositions bs = if sq /= Nothing then Set.toList $ Set.difference (foldl (Set.union) (Set.fromList []) [Set.fromList altpos | bToSee <- movesCmp, let altpos = getPiecePositions bToSee (fromJust sq)]) (Set.fromList $ getPiecePositions b (fromJust sq)) else []
  where b = board bs
        cmp :: Maybe SquarePos
        cmp = if squareSelected bs == Nothing then currentMousePos bs else squareSelected bs
        sq :: Maybe Square
        sq = if cmp /= Nothing then Just $ getSquare (fromJust cmp) b else Nothing
        movesCmp :: [Board]
        movesCmp = if sq /= Nothing then pos_move (board bs) (fromJust sq) (fromJust cmp) White else []
                    


main :: IO ()
main = do
  Gloss.play window backGround 60 initialState renderGame handleEvent playGame
