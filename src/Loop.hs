-- | Модуль, отвечающий за передачу и обработку комманд от пользователя
module Loop where

import Game

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
gameLoop = un



-- | функция заглушка, типа TODO для хаскела
un :: a
un = undefined
