module Main where

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Game as Game
import Board

width, height, offset :: Int
width = 800
height = 600
offset = 10

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


mkBoard :: [Gloss.Picture]
mkBoard = [(Gloss.translate (fromIntegral (x)) (fromIntegral (y)) $ Gloss.color (if ((x `div` fator `mod` 2 == 1 && y `div` fator `mod` 2 == 1) || (x `div` fator `mod` 2 == 0 && y `div` fator `mod` 2 == 0)) then (Gloss.greyN 0.5) else Gloss.white) $ Gloss.rectangleSolid (fromIntegral fator)  (fromIntegral fator)) | x <- (map (*fator) [1..8]), y <- (map (*fator) [1..8])]

mkPieces bs = mkPieces' (0,0) (board bs)
  where mkPieces' :: SquarePos -> Board -> [Gloss.Picture]
        mkPieces' (i,j) b = Gloss.translate (fromIntegral ((j+1)*fator)) (fromIntegral ((i+1)*fator)) (mkPiece (getSquare (i,j) b)) : (if (i < 7) then mkPieces' (i+1, j) b else []) ++ (if (j < 7) then mkPieces' (i, j+1) b else [])
        mkPiece :: Square -> Gloss.Picture
        mkPiece (Just (Piece (Pawn _) c)) = if c == White then whitePawn else blackPawn
        mkPiece (Just (Piece (Rook) c)) = if c == White then whiteRook else blackRook
        mkPiece (Just (Piece (Bishop) c)) = if c == White then whiteBishop else blackBishop
        mkPiece (Just (Piece (Knight) c)) = if c == White then whiteKnight else blackKnight
        mkPiece (Just (Piece (Queen) c)) = if c == White then whiteQueen else blackQueen
        mkPiece (Just (Piece (King) c)) = if c == White then whiteKing else blackKing
        mkPiece Nothing = Gloss.blank

data BoardState = Game { board :: Board }

render :: BoardState -> Gloss.Picture
render game = Gloss.translate (-4.5*(fromIntegral fator)) (-4.5*(fromIntegral fator)) $ Gloss.scale 1 1 $ Gloss.pictures $ (mkBoard ++ (mkPieces game))

initialState :: BoardState
initialState = Game { board = initialBoard }

main :: IO ()
main = do
  Gloss.display window backGround $ render initialState
