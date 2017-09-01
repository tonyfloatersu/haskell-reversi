module PVPGameM ( pvpGame ) where

import           BoardM
import           BasicM

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
