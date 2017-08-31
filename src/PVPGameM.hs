module PVPGameM where

import           BoardM
import           BasicM
import qualified Data.Maybe as DM

pvp :: IO ()
pvp    = undefined

pvpFunction :: Board -> IO Board
pvpFunction b    = undefined

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
