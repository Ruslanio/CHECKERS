module NavalBattle where

import Graphics.Gloss.Interface.Pure.Game
import Data.List (transpose)

runBattle:: IO()
runTicTacToe = do
  play display bgColor fps initGame drawGame handleGame updateGame
  where
    display = InWindow "Шашки" (screenWidth, screenHeight) (200, 200)
    bgColor = white   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду


-- | Шашки игроков.
data Mark = X | O            -- ^ X - светлые, O - темные.
  deriving (Eq, Show)

-- | Клетка игрового поля.
type Cell = Maybe Mark

-- | Игровое поле.
type Board = [[Cell]]

-- | Состояние игры.
data Game = Game
  { gameBoard  :: Board       -- ^ Игровое поле.
  , gamePlayer :: Mark        -- ^ Чей ход?
  , gameWinner :: Maybe Mark  -- ^ Победитель.
  }


-- | Начальное состояние игры.
-- Начальное расположение шашек
-- Первые ходят светлые .
initGame :: Game
initGame = Game
  { gameBoard  = placeMarksForStart (replicate boardHeight (replicate boardWidth Nothing))
  , gamePlayer = X
  , gameWinner = Nothing
  }


-- | Ширина игрового поля в клетках.
boardWidth :: Int
boardWidth  = 8

-- | Высота игрового поля в клетках.
boardHeight :: Int
boardHeight = 8

-- | Размер одной клетки в пикселях.
cellSize :: Int
cellSize = 100

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = cellSize * boardWidth

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = cellSize * boardHeight

-- =========================================
-- Тут рисуем
-- =========================================

-- | Отобразить игровое поле.
drawGame :: Game -> Picture
drawGame game = translate (-w) (-h) (scale c c (pictures
  [ drawGrid
  , drawBoard (gameWinner game) (gameBoard game)
  ]))
  where
    c = fromIntegral cellSize
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Сетка игрового поля.
drawGrid :: Picture
drawGrid = color black (pictures (hs ++ vs))
  where
    hs = map (\j -> line [(0, j), (n, j)]) [1..m - 1]
    vs = map (\i -> line [(i, 0), (i, m)]) [1..n - 1]

    n = fromIntegral boardWidth
    m = fromIntegral boardHeight


-- | Нарисовать шашки на игровом поле.
drawBoard :: Maybe Mark -> Board -> Picture
drawBoard win board = pictures (map pictures drawCells)
  where
    drawCells = map drawRow (zip [0..] board)           -- нумеруем Cells
    drawRow (j, row) = map drawCellAt (zip [0..] row)
      where
        drawCellAt (i, cell) = translate (0.5 + i) (0.5 + j)
          (drawCell win cell)

-- | Нарисовать шашку в клетке поля (если она там есть).
drawCell :: Maybe Mark -> Cell -> Picture
drawCell Nothing = blank
drawCell win (Just mark)
  = color markColor (drawMark mark)
  where
    markColor
      | win == X mark = light orange
      | win == O mark = greyN 0.5

drawMark :: Mark -> Picture
drawMark = circleSolid 0.3









doShit :: (Int, Int) -> Game


-- | Обновление игры.
-- В этой игре все изменения происходят только по действиям игрока,
-- поэтому функция обновления — это тождественная функция.
updateGame :: Float -> Game -> Game
updateGame _ = id


placeMarksForStart :: Board -> Board
placeMarksForStart board =