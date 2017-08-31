module BoardM where

import           BasicM
import qualified Data.Maybe as DM

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
                                   ++ [ Just Hand { loc = (3, 3), clr = White }
                                      , Just Hand { loc = (3, 4), clr = Black } ]
                                   ++ replicate 3 Nothing
                          , Line $ replicate 3 Nothing
                                   ++ [ Just Hand { loc = (4, 3), clr = Black }
                                      , Just Hand { loc = (4, 4), clr = White } ]
                                   ++ replicate 3 Nothing ]
                       ++ replicate 3 blankLine

searchPriority :: (Int, Int) -> Int
searchPriority (row, col)    = ((!! reverseData row) . (!! reverseData col))
                               [ [100, -5, 10, 5]
                               , [-5, -50, 1, 1]
                               , [10, 1, 3, 2]
                               , [5, 1, 2, 1] ] where
    reverseData :: Int -> Int
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
locToSituation l@ (_, lb) b          = (!! lb) $ locToLine l b where
    locToLine :: Location -> Board -> [Maybe Hand]
    locToLine (loa, _) (Board bs)    = (\(Line lis) -> lis) $ bs !! loa

uniqueList :: Eq a => [a] -> [a]
uniqueList []          = []
uniqueList (x : xs)    = x : uniqueList (filter (/= x) xs)

colorForNext :: Color -> Board -> [(Location, Maybe Hand)]
colorForNext c b    = uniqueList $ concat $ flip locForNext b <$> colorSearchBoard c b

locForNext :: Location -> Board -> [(Location, Maybe Hand)]
locForNext l b        = concat $ arrayCheck <$> edHLO where
     edsLocOg    = (\x -> ((:) <$> fst) x <$> snd x)
                    . origEightDirs $ l                    :: [[Location]]
     edsHdOg     = (<$>) (`locToSituation` b) <$> edsLocOg :: [[Maybe Hand]]
     edHLO       = uncurry zip <$> zip edsLocOg edsHdOg    :: [[(Location, Maybe Hand)]]

unitCheck :: Maybe Color -> (Location, Maybe Hand) -> [(Location, Maybe Hand)]
unitCheck c (loca, hm) | DM.isNothing c    = [ (loca, hm) | DM.isNothing hm ]
                       | otherwise         = [ (loca, hm) | c == retColor hm ]

arrayCheck :: [(Location, Maybe Hand)] -> [(Location, Maybe Hand)]
arrayCheck []                      = []
arrayCheck (x : xs) | condition    = []
                    | otherwise    = unitCheck Nothing (head elimneg) where
    origColor                      = retColor $ snd x :: Maybe Color
    elimneg                        = dropWhile (not . null . unitCheck (negnate origColor))
                                                xs    :: [(Location, Maybe Hand)]
    condition                      = length elimneg == length xs
                                     || null elimneg  :: Bool

{- then we start the modify part -}

takeLocation :: [(Location, Maybe Hand)] -> [Location]
takeLocation    = (<$>) fst

modify :: Board -> Maybe Color -> Location -> Board
modify (Board bs) cgTo (l1, l2)       = Board $ take l1 bs
                                                ++ [ Line $ take l2 ls
                                                            ++ [changeHand]
                                                            ++ drop (l2 + 1) ls ]
                                                ++ drop (l1 + 1) bs where
    (Line ls)                         = bs !! l1 :: Line
    changeHand | DM.isNothing cgTo    = Nothing
               | otherwise            = Just Hand { loc = (l1, l2)
                                                  , clr = DM.fromJust cgTo }

modifyTobe :: Color -> Board -> Board
modifyTobe c b    = foldr (\lc brd -> modify brd (Just Next) lc)
                           b $ takeLocation $ colorForNext c b

giveChanges :: Board -> Hand -> [Maybe Hand]
giveChanges b Hand { loc = l, clr = c }    = concat changeDirRes where
    ehtDrsTemp                             = snd . origEightDirs $ l  :: [[Location]]
    ehtDrsHands                            = (<$>) (`locToSituation` b)
                                              <$> ehtDrsTemp          :: [[Maybe Hand]]
    colorTake                              = negnate (Just c)         :: Maybe Color
    tails                                  = dropWhile (\x -> retColor x == colorTake)
                                              <$> ehtDrsHands         :: [[Maybe Hand]]
    smalljudge :: [Maybe Hand] -> Bool
    smalljudge []                          = False
    smalljudge (x : _) | DM.isNothing x    = False
                       | otherwise         = Just c == retColor x
    smallresult                            = smalljudge <$> tails     :: [Bool]
    changeDirsHands                        = takeWhile (\x -> retColor x == colorTake)
                                              <$> ehtDrsHands         :: [[Maybe Hand]]
    changeDirRes                           = (\(x, y) -> if x then y else [])
                                              <$> zip smallresult changeDirsHands

boardChanges :: Board -> Color -> [[Maybe Hand]]
boardChanges b c    = giveChanges b <$> posbFuture where
    locations       = fst <$> colorForNext c b                        :: [Location]
    posbFuture      = (\l -> Hand { loc = l, clr = c }) <$> locations :: [Hand]

statColorChanges :: Board -> Color -> [(Location, [Maybe Hand])]
statColorChanges b c    = zip (fst <$> colorForNext c b) $ boardChanges b c

modifyH :: Hand -> Board -> Board
modifyH h@ Hand { loc = _, clr = c } b    = foldr fragfunc b locats where
    change                                = Just h : giveChanges b h :: [Maybe Hand]
    locats                                = DM.fromJust . retLocation
                                             <$> change              :: [Location]
    fragfunc :: Location -> Board -> Board
    fragfunc lc brd                       = modify brd (Just c) lc

{- used for debug not for real useage -}

varChange :: (Int, Int) -> Char -> Block
varChange (y, x) c | c == 'O'     = Just Hand { loc = (y, x), clr = White }
                   | c == 'X'     = Just Hand { loc = (y, x), clr = Black }
                   | c == ' '     = Nothing
                   | otherwise    = error "WTF!!"

lineTrans :: Int -> String -> Line
lineTrans i str    = Line $ (\x -> varChange (i, x) (str !! x)) <$> [0 .. 7]

testingFunc :: [String] -> Board
testingFunc xs    = Board $ (\x -> lineTrans x (xs !! x)) <$> [0 .. 7]

prototype :: [String]
prototype    = [ "        "
               , "        "
               , "    X   "
               , "  OOXX  "
               , "   OXOX "
               , "    X   "
               , "    X   "
               , "        " ]

data Situation = Situation { black :: Int
                           , white :: Int }

instance Show Situation where
    show Situation { black = b, white = w }    = "current situation: " ++ "\n"
                                                  ++ "black: " ++ show b ++ "\t"
                                                  ++ "white: " ++ show w
                                                  ++ if b > w
                                                     then "\nblack number is greater"
                                                     else "\nwhite number is greater"

provideSituation :: Board -> Situation
provideSituation b    = Situation { black = length $ colorSearchBoard Black b
                                  , white = length $ colorSearchBoard White b }

endingJudgement :: Board -> Bool
endingJudgement b    =  null (colorForNext Black b) || null (colorForNext White b)
