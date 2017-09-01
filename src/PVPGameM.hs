module PVPGameM where

import           BoardM
import           BasicM
import qualified Data.Maybe as DM

pvpGame :: IO ()
pvpGame    = (print . modifyTobe White $ boardInit) >> pvpFake boardInit

pvpFake :: Board -> IO ()
pvpFake b | endingJudgement b    = return ()
          | otherwise            = fragment b >>= pvpFake

{- first white then black -}

fragment :: Board -> IO Board
fragment b    = promptChessAndShow "give me a white one" White b
                >>= (\x -> (print . modifyTobe Black $ x) >> return x)
                >>= promptChessAndShow "give me a black one" Black
                >>= (\x -> (print . modifyTobe White $ x) >> return x)

promptChessAndShow :: String -> Color -> Board -> IO Board
promptChessAndShow pt c b    = modfyJdg where
    fallCond    = not . null $ colorForNext c b                           :: Bool
    locaPrpt    = promptLocation pt $ nextMoveJudge b c                   :: IO Location
    handFall    = (\l -> Hand { loc = l, clr = c }) <$> locaPrpt          :: IO Hand
    modfyJdg    = if fallCond then (`modifyH` b) <$> handFall
                  else putStrLn "no location for you to fall" >> return b :: IO Board

readLocationMaybe :: IO (Maybe Location)
readLocationMaybe    = (fst <$>) . DM.listToMaybe
                                 . (reads :: String -> [(Location, String)])
                                <$> getLine

promptLocationM :: String -> (Maybe Location -> Bool) -> IO (Maybe Location)
promptLocationM str    = prompt str readLocationMaybe

promptLocation :: String -> (Maybe Location -> Bool) -> IO Location
promptLocation str func    = DM.fromJust <$> promptLocationM str func

{- a function to get from Color Next Location array for location take and parse -}

nextMoveJudge :: Board -> Color -> Maybe Location -> Bool
nextMoveJudge b c (Just l)    = l `elem` (fst <$> colorForNext c b)
nextMoveJudge _ _ Nothing     = False
