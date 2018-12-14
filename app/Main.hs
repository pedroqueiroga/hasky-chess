module Main where

import qualified Graphics.Gloss as Gloss
import Board

width, height, offset :: Int
width = 800
height = 600
offset = 10

window :: Gloss.Display
window = Gloss.InWindow
  "hasky-chess"
  (width, height)
  (offset, offset)

backGround :: Gloss.Color
backGround = Gloss.black

mkBoard :: [Gloss.Picture]
mkBoard = [(Gloss.translate (fromIntegral (x-(4*fator))) (fromIntegral (y-(4*fator))) $ Gloss.color (if ((x `div` fator `mod` 2 == 1 && y `div` fator `mod` 2 == 1) || (x `div` fator `mod` 2 == 0 && y `div` fator `mod` 2 == 0)) then Gloss.black else Gloss.white) $ Gloss.rectangleSolid (fromIntegral fator)  (fromIntegral fator)) | x <- (map (*fator) [1..8]), y <- (map (*fator) [1..8])]
  where fator = (width `div` 40) :: Int

data BoardState = Game { board :: Board }

render :: BoardState -> Gloss.Picture
render game = Gloss.translate 0 0 $ Gloss.scale 2 2 $ Gloss.pictures $ mkBoard

initialState :: BoardState
initialState = Game { board = initialBoard }

main :: IO ()
main = do
  Gloss.display window backGround $ render initialState
