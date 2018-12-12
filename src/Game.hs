-- | Модуль, отвечающий за логику игры
module Game where

import Data.Array
import Util

data Move = Up | Down | Left | Right
    deriving (Enum)

type Label = Int

type Pos = (Int, Int)

type Board = Array Pos Label

data Game = Game {
        emptyField  :: Pos,
        gameBoard   :: Board }

shuffle :: Int -> IO Game
shuffle = un

isGameOver :: Game -> Bool
isGameOver = un