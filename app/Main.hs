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
mkBoard = replicate 4 (Gloss.translate 0 0 $ Gloss.color Gloss.black $ Gloss.rectangleSolid 10 10) ++ replicate 4 (Gloss.translate 10 10 $ Gloss.color Gloss.white $ Gloss.rectangleSolid 10 10)

data BoardState = Game { board :: Board }

render :: BoardState -> Gloss.Picture
render game = Gloss.translate 0 0 $ Gloss.scale 0 0 $ Gloss.pictures $ mkBoard

initialState :: BoardState
initialState = Game { board = initialBoard }

main :: IO ()
main = do
  Gloss.display window backGround $ render initialState
