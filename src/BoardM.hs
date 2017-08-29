module BoardM where

import           BasicM
import qualified Data.Maybe as DM
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

{- this is for useage, for the opening of the game -}

boardInit :: Board
boardInit    = Board $ replicate 3 blankLine
                       ++ [ Line $ replicate 3 Nothing
                                   ++ [ Just Hand { loc = (4, 4), clr = White }
                                      , Just Hand { loc = (4, 5), clr = Black } ]
                                   ++ replicate 3 Nothing
                          , Line $ replicate 3 Nothing
                                   ++ [ Just Hand { loc = (5, 4), clr = Black }
                                      , Just Hand { loc = (5, 5), clr = White } ]
                                   ++ replicate 3 Nothing ]
                       ++ replicate 3 blankLine

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

{- direction search module -}

oneDirHSearch :: Location -> Vector -> [Location]
oneDirHSearch l v | isInsidePlate l    = l : oneDirHSearch (pairOper (+) l v) v
                  | otherwise          = []

oneDirectionSearch :: Location -> Vector -> Maybe [Location]
oneDirectionSearch l v | null $ tail $ oneDirHSearch l v    = Nothing
                       | otherwise                          = Just $ tail $ oneDirHSearch l v

eightDirs :: Location -> [Maybe [Location]]
eightDirs l    = oneDirectionSearch l <$> dirVec

origEightDirs :: Location -> (Location, [[Location]])
origEightDirs l    = (l, DM.fromMaybe [] <$> eightDirs l)

origEightDirsSet :: Location -> (Location, [Location])
origEightDirsSet l    = (l, eightDirs l >>= DM.fromMaybe [])

{- color search module -}

colorSearchBlockM :: Color -> Block -> Maybe Location
colorSearchBlockM c b | Just c == retColor b    = retLocation b
                      | otherwise               = Nothing

colorSearchBlock :: Color -> Block -> [Location]
colorSearchBlock c b | DM.isJust $ colorSearchBlockM c b    = [DM.fromJust $ retLocation b]
                     | otherwise                            = []

colorSearchLine :: Color -> Line -> [Location]
colorSearchLine c (Line ls)    = ls >>= colorSearchBlock c

colorSearchBoard :: Color -> Board -> [Location]
colorSearchBoard c (Board lsls)    = lsls >>= colorSearchLine c

locToSituation :: Location -> Board -> Maybe Hand
locToSituation l@ (_, lb) b    = (!! lb) $ locToLine l b
    where locToLine :: Location -> Board -> [Maybe Hand]
          locToLine (loa, _) (Board bs)    = (\(Line lis) -> lis) $ bs !! loa

locForNext :: Location -> Board -> [Location]
locForNext l b    = undefined
    where currentHand      = DM.fromJust $ locToSituation l b             :: Hand
          currentColor     = DM.fromJust $ retColor $ Just currentHand    :: Color
          eightDirsOrig    = (\x -> ((:) <$> fst) x <$> snd x)
                              . origEightDirs $ l                         :: [[Location]]


{-


reversiLaw :: Color -> [Maybe Hand] -> [Location]
reversiLaw    = undefined

ifMatch :: [Maybe Hand] -> [Location]
ifMatch ls | DM.isNothing $ last ls    = []
           | otherwise                 = undefined
    where headCol :: Color
          headCol    = DM.fromJust $ retColor $ head ls

findNextByArray :: Hand -> Board -> [Location] -> [Location]
findNextByArray h@ Hand { loc = l, clr = c } b@ (Board bs) ls    = undefined

findNextByHand :: Hand -> Board -> [[Location]] -> [Location]
findNextByHand h bd ls    = findNextByArray h bd <$> ls >>= undefined

findNext :: Color -> Board -> [Location]
findNext c b    = undefined
    where colLocations :: [Location]
          colLocations    = colorSearchBoard c b
          potentialVec :: [(Location, [[Location]])]
          potentialVec    = origEightDirs <$> colLocations

-}
