module Main where

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Game as Game
import Graphics.Gloss.Interface.IO.Game hiding (Color)
import Board
import BestMove
import MoveMaker
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

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
  , fim :: Bool
  , history :: [Board]
  , goBots :: Bool
  , elapsedTime :: Float
  }

renderGame :: TVar BoardState -> IO Gloss.Picture
renderGame game = do
  g <- readTVarIO game
  let b = board g
  return $ Gloss.translate (-4.5*(fromIntegral fator)) (-4.5*(fromIntegral fator)) $ Gloss.pictures $ ((mkBoard g) ++ (mkPieces b))

initialState :: BoardState
initialState = Game
  {
    board = initialBoard
  , currentPlayer = White
  , squareSelected = Nothing
  , currentMousePos = Nothing
  , lastMousePos = Nothing
  , currentTips = []
  , fim = False
  , history = []
  , goBots = False
  , elapsedTime = 0
  }

stepGame :: BoardState -> BoardState
stepGame bs = if fim bs == True
              then bs
                else if length (possible_moves newBoard (other c)) == 0
                     then bs { board = newBoard, currentPlayer = (other c), fim = True, currentTips = [] }
                     else bs { board = newBoard, currentPlayer = (other c), currentTips = possiblePositions bs { board = newBoard, currentPlayer = (other c), currentTips = [] } }
  where newBoard = bestMove maxDepth (board bs) c
        c = currentPlayer bs

maxDepth :: Int
maxDepth = 4

playGame :: Float -> TVar BoardState -> IO (TVar BoardState)
playGame elapTime mbs = return mbs {-return $ if goBots bs
                                then if ((elapsedTime bs) > 1)
                                     then stepGame bs { elapsedTime = 0 }
                                     else bs { elapsedTime = elapsedTime bs + elapTime }
                                else if currentPlayer bs == Black
                                     then stepGame bs else bs-}

handleEvent :: Event -> TVar BoardState -> IO (TVar BoardState)
handleEvent (EventKey (MouseButton LeftButton) Down _ mp@(x,y)) mbs
  = do
  bs <- readTVarIO mbs
  let c = currentPlayer bs
  let mb = moveBoard (fromJust (squareSelected bs)) (fromPixel mp) c (board bs)
  if fim bs == True then atomically (do
                                        writeTVar mbs initialState
                                        return mbs
                                    )
    else
      if (squareSelected bs) == Nothing
      then atomically (do
                          writeTVar mbs bs { squareSelected = if currentTips bs /= [] then (Just (fromPixel mp)) else Nothing, currentTips = possiblePositions bs }
                          return mbs
                      )
      else if mb == Nothing
           then atomically (do
                               writeTVar mbs bs { squareSelected = Nothing, currentTips = possiblePositions bs { squareSelected = Nothing } }
                               return mbs
                           )
           else if length (possible_moves (fromJust mb) (other c)) == 0
                then atomically (do
                                    writeTVar mbs bs { squareSelected = Nothing, board = fromJust mb, fim = True, currentTips = [] }
                                    return mbs
                                )
                else atomically (do
                                    writeTVar mbs bs { squareSelected = Nothing, board = fromJust mb, currentPlayer = other c, currentTips = [], history = (board bs):(history bs) }
                                    return mbs
                     )
  where 

handleEvent (EventMotion mp@(x,y)) mbs
  = do
  bs <- readTVarIO mbs
  let bmp@(bmpx, bmpy) = fromPixel mp
  let curPos = if bmpx > 7 || bmpx < 0 || bmpy > 7 || bmpy < 0 then Nothing else Just bmp
  if lastMousePos bs /= curPos
    then atomically (do
                        writeTVar mbs bs { lastMousePos = currentMousePos bs, currentMousePos = curPos, currentTips = if fim bs == True then [] else possiblePositions bs }
                        return mbs
                    )
    else atomically (do
                        writeTVar mbs bs
                        return mbs
                    )

handleEvent (EventKey (MouseButton RightButton) Down _ mp@(x,y)) mbs
  = (do
        bs <- readTVarIO mbs
        if fim bs == True
          then atomically (do
                              writeTVar mbs initialState
                              return mbs
                          )
          else atomically (do
                              writeTVar mbs bs { squareSelected = Nothing, currentTips = possiblePositions bs { squareSelected = Nothing } }
                              return mbs
                          )
    )

handleEvent (EventKey (Char 'r') Up _ _) mbs = atomically (
  do
    writeTVar mbs initialState
    return mbs
  )
handleEvent (EventKey (Char 'b') Up _ _) mbs = do
  bs <- readTVarIO mbs
  atomically (
    do
      writeTVar mbs bs { goBots = True }
      return mbs
    )
handleEvent (EventKey (Char 'p') Up _ _) mbs = do
  bs <- readTVarIO mbs
  atomically (do
                 writeTVar mbs bs { goBots = False }
                 return mbs
             )

handleEvent (EventKey (Char 'u') Up _ _) mbs = (
  do
    bs <- readTVarIO mbs
    let head:tail = history bs
    if history bs == [] then return mbs
      else atomically (
      do
        writeTVar mbs bs { board = head, history = tail, fim = False, elapsedTime = 0 }
        return mbs
      )
  )

handleEvent _ bs = return bs

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
                       , let xij = x !! i' !! j'
                       , rlxEqPcs xij piece0 ||
                         (i0 == 6 && i' == 7 && eqPromoted xij piece0)
                       , x !! i0 !! j0 == Nothing]
                    where 

possiblePositions :: BoardState -> [SquarePos]
possiblePositions bs = if sq /= Nothing then Set.toList $ Set.difference (foldl (Set.union) (Set.fromList []) [Set.fromList altpos | bToSee <- movesCmp, let altpos = getPiecePositions bToSee (fromJust sq)]) (Set.fromList $ getPiecePositions b (fromJust sq)) else []
  where b = board bs
        cmp :: Maybe SquarePos
        cmp = if squareSelected bs == Nothing then currentMousePos bs else squareSelected bs
        sq :: Maybe Square
        sq = if cmp /= Nothing then Just $ getSquare (fromJust cmp) b else Nothing
        movesCmp :: [Board]
        movesCmp = if sq /= Nothing then pos_move (board bs) (fromJust sq) (fromJust cmp) (currentPlayer bs) else []



main :: IO ()
main = do
  sharedState <- newTVarIO initialState
  forkIO $ thr sharedState
  playIO window backGround 30 sharedState renderGame handleEvent playGame
  return ()

thr :: TVar BoardState -> IO ()
thr mbs = forever $ do
  threadDelay 1000000
  bs <- readTVarIO mbs
  if goBots bs
    then atomically (do
                        writeTVar mbs $ stepGame bs { elapsedTime = 0 }
                    )
    else if currentPlayer bs == Black
         then atomically (do
                             writeTVar mbs $ stepGame bs
                         )
         else return ()
