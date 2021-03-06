-- | Модуль, отвечающий за логику игры
module Game where
import Prelude hiding (Either(..))
import System.Random
import Data.Array
import Util
import Debug.Trace
import System.IO.Unsafe

data Move = Up | Down | Left | Right
    deriving (Enum)

type Label = Int

type Pos = (Int, Int)

type Board = Array Pos Label

data Game = Game {
        emptyField  :: Pos,
        gameBoard   :: Board } deriving (Eq)

shuffle :: Int -> IO Game
shuffle n = (iterate (shuffle1 =<<) $ pure initGame) !! n

shuffle1 :: Game -> IO Game
shuffle1 g = flip move g <$> (randomElem $ nextMoves g)

-- генерируем случайное число в диапазоне индексов списка и затем извлекаем элемент.
randomElem :: [a] -> IO a
randomElem xs = (xs !! ) <$> randomRIO (0, length xs - 1)

nextMoves g = filter (within . moveEmptyTo . orient) allMoves
    where moveEmptyTo v = shift v (emptyField g)
          allMoves = [Up, Down, Left, Right]

isGameOver :: Game -> Bool
isGameOver = ( == initGame)

newtype Vec = Vec (Int, Int)

-- Надстройка над move для работы с нрафической оболочкой
moveGraphic :: Pos -> Game -> Game
moveGraphic pos game = case pointDirection pos game of
        Nothing -> game
        Just direction -> move direction game


-- Направления не совпадают из-за несовпадения разметки массива с системой координат в пакете Gloss
pointDirection :: Pos -> Game -> Maybe Move
pointDirection pos game
             | pos == left = Just Left
             | pos == right = Just Right
             | pos == down = Just Down
             | pos == up = Just Up
             | otherwise = Nothing
            where left = shift (orient Left) (emptyField game)
                  right = shift (orient Right) (emptyField game)
                  down = shift (orient Down) (emptyField game)
                  up = shift (orient Up) (emptyField game)

move :: Move -> Game -> Game
move m (Game id board)
    | within id' = Game id' $ board // updates
    | otherwise  = Game id board
    where id' = shift (orient m) id
          updates = [(id, board ! id'), (id', emptyLabel)]

-- определение того, что индексы внутри доски
within :: Pos -> Bool
within (a, b) = p a && p b
    where p x = x >= 0 && x <= 3

-- смещение положение по направлению
shift :: Vec -> Pos -> Pos
shift (Vec (va, vb)) (pa, pb) = (va + pa, vb + pb)

-- направление хода
orient :: Move -> Vec
orient m = Vec $ case m of
    Up      -> (-1, 0)
    Down    -> (1 , 0)
    Left    -> (0 ,-1)
    Right   -> (0 , 1)

-- метка для пустой фишки
emptyLabel :: Label
emptyLabel = 15

initGame :: Game
initGame = Game (3, 3) $ listArray ((0, 0), (3, 3)) $ [0 .. 15]


-- Вынужденная мера, не трогать, может жахнуть
initGameGraphic :: Game
initGameGraphic = unsafePerformIO $ shuffle graphicDifficulty

updateGame :: Float -> Game -> Game
updateGame _ = id

instance Show Game where
  show (Game _ board) = "\n" ++ space ++ line ++
          (foldr (\a b -> a ++ space ++ line ++ b) "\n" $ map column [0 .. 3])
          where post id = showLabel $ board ! id
                showLabel n  = cell $ show $ case n of
                          15 -> 0
                          n  -> n+1
                cell "0"   = "    "
                cell [x]   = ' ':' ': x :' ':[]
                cell [a,b] = ' ': a : b :' ':[]
                line = "+----+----+----+----+\n"
                nums = ((space ++ "|") ++ ) . foldr (\a b -> a ++ "|" ++ b) "\n".
                          map post
                column i = nums $ map (\x -> (i, x)) [0 .. 3]
                space = "\t"


-- Сложность игры для графической версии
graphicDifficulty :: Int
graphicDifficulty = 5