module BoardM where

import           BasicM

type Block = Hand

newtype Line = Line [Block]

newtype Board = Board [Line]

boardLine :: String
boardLine    = "+---+---+---+---+---+---+---+---+\n"

instance Show Line where
    show (Line b)    = concat ((\Hand { loc = _, clr = c } -> "| " ++ show c ++ " ") <$> b)
                       ++ "|\n"

instance Show Board where
    show (Board l)    = concat ((\x -> boardLine ++ show x) <$> l) ++ boardLine

{- just used for test, not for real usage -}

blankLine :: Line
blankLine    = Line $ replicate size Hand { loc = (1, 2), clr = NULL }

blankBoard :: Board
blankBoard    = Board $ replicate size blankLine
