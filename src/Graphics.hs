module Graphics where

import Graphics.Gloss.Interface.Pure.Game
import Data.List (transpose)
import Data.Array (assocs)
import Game
import Util


type Cell = Maybe Label


-- "Переменные"
-- | Ширина игрового поля в клетках.
boardWidth :: Int
boardWidth  = 4

-- | Высота игрового поля в клетках.
boardHeight :: Int
boardHeight = 4

-- | Размер одной клетки в пикселях.
cellSize :: Int
cellSize = 100

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = cellSize * boardWidth

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = cellSize * boardHeight


-- | Запустить игру «Пятнашки».
run :: IO ()
run = do
  play display bgColor fps initGame drawGame handleGame updateGame
  where
    display = InWindow "Пятнашки" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду


drawGame :: Game -> Picture
drawGame game = translate (-w) (-h) (scale c c (pictures
  [ drawGrid, drawBoard game
  ]))
  where
    c = fromIntegral cellSize
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

drawBoard :: Game -> Picture
drawBoard game = pictures drawCells
  where
    drawCells = map drawRow (assocs $ gameBoard game)

drawRow :: ((Int,Int), Label) -> Picture
drawRow ((i,j), label) = translate (0.4 + fromIntegral j) (0.4 + fromIntegral i) (drawCell label)

-- | Нарисовать цифру в клетке поля.
drawCell :: Label -> Picture
drawCell label  = color white (drawLabel label)

-- | Нарисовать цифру.
drawLabel :: Label -> Picture
drawLabel label
          | label < 15 = scale 0.002 0.002 . text $ (show label)
          | otherwise = blank

-- | Сетка игрового поля.
drawGrid :: Picture
drawGrid = color white (pictures (hs ++ vs))
  where
    hs = map (\j -> line [(0, j), (n, j)]) [1..m - 1]
    vs = map (\i -> line [(i, 0), (i, m)]) [1..n - 1]

    n = fromIntegral boardWidth
    m = fromIntegral boardHeight

-- | Обработка событий.
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) Graphics.Gloss.Interface.Pure.Game.Down _ mouse) = makeMove (mouseToCell mouse)
handleGame _ = id
-- handleGame (EventKey (MouseButton LeftButton) Graphics.Gloss.Interface.Pure.Game.Down _ mouse) = move (mouseToCell mouse)
-- handleGame _ = id


-- | Получить координаты клетки под мышкой.
mouseToCell :: Point -> Pos
mouseToCell (x, y) = (i, j)
  where
    i = floor (x + fromIntegral screenWidth  / 2) `div` cellSize
    j = floor (y + fromIntegral screenHeight / 2) `div` cellSize

