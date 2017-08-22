module BasicM where

size :: Int
size    = 8

data Player = Computer | Human
            deriving ( Eq, Enum )

instance Show Player where
    show p    = [ "AI", "Human" ] !! fromEnum p

data Color = Black | White | NULL
           deriving ( Eq, Enum )

instance Show Color where
    show c    = ["x", "o", " "] !! fromEnum c

negnate :: Color -> Color
negnate c | c == White    = Black
          | c == NULL     = NULL
          | otherwise     = White

type Location = ( Int, Int )

data Hand = Hand { loc :: Location
                 , color :: Color }

instance Eq Hand where
    Hand { loc = l1, color = c1 } == Hand { loc = l2, color = c2 }    = l1 == l2 && c1 == c2

instance Show Hand where
    show Hand { loc = l, color = c }    = "(" ++ concat ([ show . fst
                                                         , const ", "
                                                         , show . snd ] <*> [l])
                                          ++ ", " ++ show c ++ ")"

prompt :: String -> (String -> Bool) -> IO String
prompt command judgef    = putStrLn command >> getLine >>=
                           \x -> if judgef x
                                 then return x
                                 else prompt command judgef
