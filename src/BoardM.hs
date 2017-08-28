module BoardM where

import           BasicM
import           Control.Monad.ST
import qualified Data.Map as M
import qualified Data.List as L

type Block = Maybe Hand

newtype Line = Line [Block]

newtype Board = Board [Line]

boardLine :: String
boardLine    = ([ 1 :: Int .. 8 ] >>= const "+---") ++ "+\n"

showL :: Maybe Hand -> String
showL (Just Hand { loc = _, clr = c })    = show c
showL _                                   = " "

instance Show Line where
    show (Line b)    = (b >>= \c -> "| " ++ showL c ++ " ") ++ "|\n"

instance Show Board where
    show (Board l)    = (l >>= \x -> boardLine ++ show x) ++ boardLine

{- just used for test, not for real usage -}

blankLine :: Line
blankLine    = Line $ replicate size Nothing

blankBoard :: Board
blankBoard    = Board $ replicate size blankLine

searchPriority :: (Int, Int) -> Int
searchPriority (row, col)    = ((!! reverseData row) . (!! reverseData col))
                               [ [100, -5, 10, 5]
                               , [-5, -50, 1, 1]
                               , [10, 1, 3, 2]
                               , [5, 1, 2, 1] ]
    where reverseData :: Int -> Int
          reverseData x    = if x > 3 then 7 - x else x

type Vector = (Int, Int)

dirVec :: [Vector]
dirVec    = [ (-1, -1), (-1, 1), (-1, 0), (1, 0)
            , (1, 1), (1, -1), (0, 1), (0, -1) ]

pairOper :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
pairOper f (va, vb) (vc, vd)    = (f va vc, f vb vd)

oneDirHSearch :: Location -> Vector -> [Location]
oneDirHSearch l v | isInsidePlate l    = l : oneDirHSearch (pairOper (+) l v) v
                  | otherwise          = []

oneDirectionSearch :: Location -> Vector -> Maybe [Location]
oneDirectionSearch l v | null $ tail $ oneDirHSearch l v    = Nothing
                       | otherwise                          = Just $ tail $ oneDirHSearch l v

eightDirs :: Location -> [Maybe [Location]]
eightDirs l    = oneDirectionSearch l <$> dirVec
