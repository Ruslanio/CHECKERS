-- | Модуль, отвечающий за передачу и обработку комманд от пользователя
module Loop where

import Game
import Util

data Query = Quit | NewGame Int | Play Move

play :: IO ()
play = greetings >> setup >>= gameLoop

greetings :: IO ()
greetings = un

setup :: IO Game
setup = putStrLn "Начнём новую игру?" >>
    putStrLn "Укажите сложность (положительное целое число): " >>
    getLine >>= maybe setup shuffle . readInt                      -- | '.' значит что мы вычисляеи байду справа и пихаем то что получилось в байду слева

readInt :: String -> Maybe Int
readInt = un

gameLoop :: Game -> IO ()
gameLoop game
    | isGameOver game   = showResults game >> setup >>= gameLoop
    | otherwise         = showGame game >> askForMove >>= reactOnMove game


showResults :: Game -> IO ()
showResults = un

showGame :: Game -> IO ()
showGame = un

askForMove :: IO Query
askForMove = un

reactOnMove :: Game -> Query -> IO ()
reactOnMove = un
