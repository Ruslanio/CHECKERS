-- | Модуль, отвечающий за передачу и обработку комманд от пользователя
module Loop where
import Prelude hiding (Either(..))
import Game
import Util
import Data.Char (isDigit)

data Query = Quit | NewGame Int | Play Move

play :: IO ()
play = greetings >> setup >>= gameLoop



setup :: IO Game
setup = putStrLn "Начнём новую игру?" >>
    putStrLn "Укажите сложность (положительное целое число): " >>
    getLine >>= maybe setup shuffle . readInt                      -- | '.' значит что мы вычисляеи байду справа и пихаем то что получилось в байду слева



gameLoop :: Game -> IO ()
gameLoop game
    | isGameOver game   = showResults game >> setup >>= gameLoop
    | otherwise         = showGame game >> askForMove >>= reactOnMove game


remindMoves :: IO ()
remindMoves = mapM_ putStrLn talk
    where talk = [
            "Возможные ходы пустой клетки:",
            "   left     или l       -- налево",
            "   right    или r       -- направо",
            "   up       или u       -- вверх",
            "   down     или d       -- вниз",
            "Другие действия:",
            "   new int  или n int -- начать новую игру, int - целое число,",
                                      "указывающее на сложность",
            "   quit     или q      -- выход из игры"]




reactOnMove :: Game -> Query -> IO ()
reactOnMove game query = case query of
    Quit        -> quit
    NewGame n   -> gameLoop =<< shuffle n
    Play    m   -> gameLoop $ move m game                           -- | '$' просто убирает скобки (одну там где стоит, вторую на конце строки)

askForMove :: IO Query
askForMove = showAsk >>
    getLine >>= maybe askAgain return . parseQuery
    where askAgain = wrongMove >> askForMove


parseQuery :: String -> Maybe Query
parseQuery x = case x of
    "up"    -> Just $ Play Up
    "u"     -> Just $ Play Up
    "down"  -> Just $ Play Down
    "d"     -> Just $ Play Down
    "left"  -> Just $ Play Left
    "l"     -> Just $ Play Left
    "right" -> Just $ Play Right
    "r"     -> Just $ Play Right
    "quit"  -> Just $ Quit
    "q"     -> Just $ Quit

    'n':'e':'w':' ':n   -> Just . NewGame =<< readInt n
    'n':' ':n           -> Just . NewGame =<< readInt n
    _       -> Nothing


readInt :: String -> Maybe Int
readInt n
    | all isDigit n = Just $ read n
    | otherwise     = Nothing

greetings :: IO ()
greetings = putStrLn "Привет! Это игра пятнашки" >>
    showGame initGame >>
    remindMoves

showAsk :: IO ()
showAsk = putStrLn "Ваш ход: "

showResults :: Game -> IO ()
showResults g = showGame g >> putStrLn "Игра окончена."

showGame :: Game -> IO ()
showGame = putStrLn . show

wrongMove :: IO ()
wrongMove = putStrLn "Не могу распознать ход." >> remindMoves

quit :: IO ()
quit = putStrLn "До встречи." >> return ()